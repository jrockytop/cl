/*
 * Copyright (c) 2012, Jason Wade Cox <jason@coxmountain.com>
 *                     Jon Gettler
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *   
 *   Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <sys/stat.h>

extern char *parse_args;
extern char *shebang;
extern char *build_setup;
extern char *build;

/*
 * Return a string that is the result of concatenating all the arguments.
 */
char *concatenate(char *string, ...)
{
	va_list ap;
	char *result = strdup(string);
	char *s;

	va_start(ap, string);

	while ((s=va_arg(ap, char*)) != NULL) {
		result = realloc(result, strlen(result)+strlen(s)+1);
		if (result == NULL) {
			break;
		}
		strcat(result, s);
	}

	va_end(ap);

	return result;
}

/*
 * Start up a lisp repl with rlwrap.   
 * This function takes a string with the <lisp> implementation to invoke
 */
void invoke_lisp_repl(char *lisp)
{
	if (!strcmp("dumb", getenv("TERM"))) {
		execlp(lisp, lisp, NULL);
	}

	if (!strcmp("sbcl", lisp)) {
		execlp("rlwrap", "sbcl", "sbcl", "--eval", shebang, "--eval", parse_args, NULL);
	}
	
	if (!strcmp("ccl", lisp) || !strcmp("ccl64", lisp)) {
		execlp("rlwrap", lisp, lisp, "-e", shebang, "-e", parse_args, NULL);
	}

	if (!strcmp("ecl", lisp)) {
		execlp("rlwrap", "ecl", "ecl", "-q", "-eval", shebang, "-eval", parse_args,
		       "-eval", "(setf *load-verbose* nil asdf:*asdf-verbose* nil)", NULL);
	}
	
	if (!strcmp("clisp", lisp)) {
		execlp("rlwrap", lisp, lisp, "-x", shebang, "-repl", NULL);
	}
	
	execlp("rlwrap", lisp, lisp, NULL);
}

void execute_lisp(char *lisp, char *expression, int ret)
{
	pid_t pid = 0;
	int status;

	if (ret) {
		pid = fork();
		if (pid < 0) {
			perror("fork");
			exit(-1);
		}
	}
	
	if (!pid) {
		if (!strcmp("sbcl", lisp)) {
			char *eval = concatenate("(progn ", expression, ")", NULL);
			execlp("sbcl", "sbcl", "--noinform", "--disable-debugger", 
			       "--eval", eval, NULL);
		}
	
		if (!strcmp("ccl", lisp) || !strcmp("ccl64", lisp)) {
			char *nodebug = concatenate("(progn (setf *debugger-hook* "
						    "(lambda (x y)"
						    "(declare (ignore y)) "
						    "(setf *print-pretty* t)(describe x)(quit)))",
						    expression, ")", NULL);
			execlp(lisp, lisp, "-Q", "-e", nodebug, NULL);
		}
		
		if (!strcmp("ecl", lisp)) {
			char *eval = concatenate("(progn (setf *load-verbose* nil "
						 "asdf:*asdf-verbose* nil)",
						 expression, ")", NULL);
			execlp("ecl", "ecl", "-q", "-eval", eval, NULL);
		}

		if (!strcmp("clisp", lisp)) {
			char *eval = concatenate("(progn ", expression, ")", NULL);
			execlp(lisp, lisp, "-q", "-q", "-x", eval, NULL);
		}
		
		fprintf(stderr, "%s lisp not supported\n", lisp);
		exit(-1);
	} else {
		pid = wait(&status);
		if (pid < 0)
			perror("wait");
	}
}

/*
 * Run a script with the passed in <lisp> implementation.
 * The <script> argument contains the scripts filename
 * The <args> argument contains the scripts argument list.
 */
void run_script(char *lisp, char *script, char *args)
{
	char *expressions = concatenate("(defvar *program-name* nil)(defvar *args* nil)",
					"(setf *program-name* \"",script, "\")", args,
					shebang, parse_args,
					"(load \"", script, "\")(if (fboundp 'main) (main))"
					"(quit)", NULL);
	execute_lisp(lisp, expressions, 0);
}

/*
 * This function will build an executable file named <execname> from
 * the lisp file <lisp_file> using the <lisp> implementation.  If the
 * <keep> option is true, the temporary build source file will not be
 * deleted (so one can inspect the code...)
 */
void build_executable(char *lisp, char *lisp_file, char *execname, int keep)
{
	char *expressions = concatenate("(defvar *program-name* nil)(defvar *args* nil)",
					shebang, parse_args, "(load \"", lisp_file, "\")",
					build_setup, "(let ((outfile \"", execname, "\"))",
					build, "(quit))", NULL);

	
	execute_lisp(lisp, expressions, 0);
}

/*
 * This function creates and returns a string
 * that will create the *args* global variable in common lisp.
 */
char *make_script_args(int argc, char **argv)
{
	int i;
	int sz;
	int offset;
	char *args;

	for (i=0, sz=0; i < argc; i++)  {
		sz += strlen(argv[i]) + 5;
		//printf("argv[%d] = '%s'\n", i, argv[i]);
	}
	sz += 32;
	args = malloc(sz);
	if (!args) {
		perror("malloc");
		exit(-1);
	}
	offset = snprintf(args, sz, "(setf *args* '(");
	for (i=0; i < argc; i++)  {
		offset += snprintf(args+offset,sz-offset,"\"%s\" ", argv[i]);
	}
	snprintf(args+offset,sz-offset,"))");
	return args;
}

int main(int argc, char **argv)
{
	static struct option long_options[] = {
		{"help",    no_argument,       0, 'h'},
		{"lisp",    required_argument, 0, 'l'},
		{"build",   required_argument, 0, 'b'},
		{"outfile", required_argument, 0, 'o'},
		{"keep",    no_argument,       0, 'k'},
		{ 0, 0, 0, 0 }
	};
	int option_index = 0;
	int c = 0;
	int keep = 0;
	char *lisp = "sbcl";
	char *execname = "a.out";
	char *build = NULL;
	char *args;

	if (getenv("CL") != NULL) {
		lisp = getenv("CL");
	}
	
	setenv("POSIXLY_CORRECT", "t", 0);
	
	while (c != -1) {
		c = getopt_long(argc, argv, "hkl:b:o:", long_options, &option_index);

		switch (c) {
			case 'h':
				printf("usage: cl [-l lisp-implementation] [-b script.lisp]"
				       " [-o outfile] [-k]\n");
				exit(0);
				break;

			case 'l':
				lisp = strdup(optarg);
				break;

			case 'b':
				build = strdup(optarg);
				break;

			case 'o':
				execname = strdup(optarg);
				break;

			case 'k':
				keep = 1;
				break;

			case '?':
				exit(1);
		}
	}

	if (optind < argc) {
		args = make_script_args(argc-optind-1, &argv[optind+1]);
		//printf("args = %s\n", args);
		run_script(lisp, argv[optind], args);
	} else {
		if (build)
			build_executable(lisp, build, execname, keep);
		else
			invoke_lisp_repl(lisp);
	}

	exit(0);
}
