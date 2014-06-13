/*
  Part of: ATCephes
  Contents: test for version functions
  Date: Fri Jun 13, 2014

  Abstract

	Test file for version functions.

  Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the COPYING file.
*/

#include <stdio.h>
#include <stdlib.h>
#include <cephes.h>

int
main (int argc, const char *const argv[])
{
  printf("version number string: %s\n", cephes_version_string());
  printf("libtool version number: %d:%d:%d\n",
	 cephes_version_interface_current(),
	 cephes_version_interface_revision(),
	 cephes_version_interface_age());
  exit(EXIT_SUCCESS);
}

/* end of file */
