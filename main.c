/*
 * Copyright (c) 2012, Jason Wade Cox <jason@coxmountain.com>
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
#include <sys/types.h>
#include <sys/wait.h>

/* The common lisp magic for #! acceptance */
char *shebang = "(set-dispatch-macro-character #\\# #\\!"
	        " (lambda (s c n)"
	        "  (declare (ignore c n))"
	        "  (read-line s nil nil t)"
	        "  nil))";

/*
 * Create a temporary file and fill it with the contents of a string.
 */
char *create_file(char *contents)
{
	char name[32];
	FILE *f;

	snprintf(name, sizeof(name), "/tmp/cl.%d.%d", getpid(), rand());

	if ((f=fopen(name, "w")) == NULL) {
		return NULL;
	}

	fprintf(f, "%s", contents);

	fclose(f);

	return strdup(name);
}

/*
 * Run a command and return with its exit status when it is finished.
 */
int run_command(char *cmd, ...)
{
	va_list ap;
	char *argv[64];
	int i = 0;
	pid_t child;
	int status;

	va_start(ap, cmd);

	do {
		argv[i++] = va_arg(ap, char*);
	} while (argv[i-1] != NULL);

	va_end(ap);

	switch ((child=fork())) {
	case -1:
		break;
	case 0:
		execvp(cmd, argv);
		break;
	default:
		while (1) {
			waitpid(child, &status, 0);
			if (WIFEXITED(status)) {
				return WEXITSTATUS(status);
			}
		}
		break;
	}

	return -1;
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
		execlp("rlwrap", "sbcl", "sbcl", "--eval", shebang, NULL);
	}
	
	if (!strcmp("ccl", lisp) || !strcmp("ccl64", lisp)) {
		execlp("rlwrap", lisp, lisp, "-e", shebang, NULL);
	}

	if (!strcmp("ecl", lisp)) {
		execlp("rlwrap", "ecl", "ecl", "-q", "-eval", shebang,
		       "-eval", "(setf *load-verbose* nil asdf:*asdf-verbose* nil)", NULL);
	}

	if (!strcmp("clisp", lisp)) {
		char *file = create_file(shebang);
		int rc;
		rc = run_command("rlwrap", lisp, lisp, "-i", file, NULL);
		unlink(file);
		exit(rc);
	}
	
	execlp("rlwrap", lisp, lisp, NULL);
}

/*
 * Run a script with the passed in <lisp> implementation.
 * The <script> argument contains the scripts filename
 * The <args> argument contains the scripts argument list.
 */
void run_script(char *lisp, char *script, char *args)
{
	int sz = strlen(script) + 32;
	char name[sz];

	snprintf(name, sz, "(defvar *program-name* \"%s\")", script);
	
	if (!strcmp("sbcl", lisp)) {
		execlp("sbcl", "sbcl", "--noinform", "--disable-debugger", "--eval", name,
		       "--eval", args, "--eval", shebang, "--load", script, "--eval", "(quit)", NULL);
	}
	
	if (!strcmp("ccl", lisp) || !strcmp("ccl64", lisp)) {
		char *nodebug = "(setf *debugger-hook* (lambda (x y)"
			"(declare (ignore y)) (describe x)(quit)))";
		
		execlp(lisp, lisp, "-Q", "-e", shebang, "-e", name, "-e", args,
		       "-e", nodebug, "-l", script, "-e", "(quit)", NULL);
	}

	if (!strcmp("ecl", lisp)) {
		execlp("ecl", "ecl", "-q",
		       "-eval", "(setf *load-verbose* nil asdf:*asdf-verbose* nil)",
		       "-eval", shebang, "-eval", name, "-eval", args,
		       "-shell", script, NULL);
	}
	
	if (!strcmp("clisp", lisp)) {
		char *f_name = create_file(name);
		char *f_args = create_file(args);
		char *f_shebang = create_file(shebang);
		int rc;
		rc = run_command(lisp, lisp, "-i", f_name, "-i", f_args,
				 "-i", f_shebang, "-q", "-q", script, NULL);
		unlink(f_name);
		unlink(f_args);
		unlink(f_shebang);
		exit(rc);
	}

	fprintf(stderr, "%s lisp not supported\n", lisp);
	exit(-1);
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
	offset = snprintf(args, sz, "(defvar *args* '(");
	for (i=0; i < argc; i++)  {
		offset += snprintf(args+offset,sz-offset,"\"%s\" ", argv[i]);
	}
	snprintf(args+offset,sz-offset,"))");
	return args;
}

int main(int argc, char **argv)
{
	static struct option long_options[] = {
		{"help", no_argument,       0, 'h'},
		{"lisp", required_argument, 0, 'l'},
		{ 0, 0, 0, 0 }
	};
	int option_index = 0;
	int c = 0;
	char *lisp = "sbcl";
	char *args;

	if (getenv("CL") != NULL) {
		lisp = getenv("CL");
	}
	
	setenv("POSIXLY_CORRECT", "t", 0);
	
	while (c != -1) {
		c = getopt_long(argc, argv, "hl:", long_options, &option_index);

		switch (c) {
			case 'h':
				printf("usage: cl [-l lisp-implementation]\n");
				exit(0);
				break;

			case 'l':
				lisp = optarg;
				break;
		}
	}

	if (optind < argc) {
		args = make_script_args(argc-optind-1, &argv[optind+1]);
		//printf("args = %s\n", args);
		run_script(lisp, argv[optind], args);
	} else {
		invoke_lisp_repl(lisp);
	}

	exit(0);
}
