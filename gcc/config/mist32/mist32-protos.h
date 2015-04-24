/* Prototypes for mist32.c functions used in the md file & elsewhere.
   Copyright (C) 2015 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Function prototypes that cannot exist in mist32.h due to dependency
   complications.  */

extern void   mist32_init (void);
extern void   mist32_init_expanders (void);
extern unsigned mist32_compute_frame_size (int);
extern void   mist32_expand_prologue (void);
extern void   mist32_expand_epilogue (void);
extern int    direct_return (void);
extern enum mist32_function_type mist32_compute_function_type (tree);
