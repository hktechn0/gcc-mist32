/* Definitions of target machine for GNU compiler, mist32 cpu.
   Copyright (C) 2012 Free Software Foundation, Inc.
   Contributed by Hirotaka Kawata <hirotaka@techno-st.net>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_MIST32_H
#define GCC_MIST32_H

/* Define this to be a string constant containing `-D' options to define the
   predefined macros that identify this machine and system.  These macros will
   be predefined unless the `-ansi' option is specified.  */

#define TARGET_CPU_CPP_BUILTINS()		\
    {						\
      builtin_define_std ("mist32");		\
      builtin_define_std ("MIST32");		\
    }						\

#define TARGET_VERSION fprintf (stderr, " (mist32)");

/* Use crt0/crtinit/crtfini files.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC  "crtend.o%s crtn.o%s"

/* Extra switches sometimes passed to the linker.  */
#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

#ifndef LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
		   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"
#endif


/* Target machine storage layout */
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Set the sizes of the core types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE DOUBLE_TYPE_SIZE

/* Define the sizes of fixed-point types.  */
#define SHORT_FRACT_TYPE_SIZE 8
#define FRACT_TYPE_SIZE 16
#define LONG_FRACT_TYPE_SIZE 32
#define LONG_LONG_FRACT_TYPE_SIZE 64

#define SHORT_ACCUM_TYPE_SIZE 16
#define ACCUM_TYPE_SIZE 32
#define LONG_ACCUM_TYPE_SIZE 64
#define LONG_LONG_ACCUM_TYPE_SIZE 64

/* long double is not a fixed mode, but the idea is that, if we
   support long double, we also want a 128-bit integer type.  */
#define MAX_FIXED_MODE_SIZE LONG_LONG_TYPE_SIZE

/* Width in bits of a pointer.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY BITS_PER_WORD

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT LONG_LONG_TYPE_SIZE

/* All accesses must be aligned.  */
#define STRICT_ALIGNMENT 1

/* Define this if you wish to imitate the way many other C compilers
   handle alignment of bitfields and the structures that contain
   them.

   The behavior is that the type written for a bit-field (`int',
   `short', or other integer type) imposes an alignment for the
   entire structure, as if the structure really did contain an
   ordinary field of that type.  In addition, the bit-field is placed
   within the structure so that it would fit within such a field,
   not crossing a boundary for it.

   Thus, on most machines, a bit-field whose type is written as `int'
   would not cross a four-byte boundary, and would force four-byte
   alignment for the whole structure.  (The alignment used may not
   be four bytes; it is controlled by the other alignment
   parameters.)

   If the macro is defined, its definition should be a C expression;
   a nonzero value for the expression enables this behavior.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* We need this for the same reason as DATA_ALIGNMENT, namely to cause
   character arrays to be word-aligned so that `strcpy' calls that copy
   constants to character arrays can be done inline, and 'strcmp' can be
   optimised to use word loads. */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) \
  DATA_ALIGNMENT (TYPE, ALIGN)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    {                                           \
      if ((MODE) == SImode)                     \
        (UNSIGNEDP) = 0;                        \
      (MODE) = Pmode;                           \
    }

/* Standard register usage.  */

/* Number of hardware registers.  We have:

   - 32 integer registers
   - condition code
   - stack pointer
   - arg pointer
*/
#define FIRST_PSEUDO_REGISTER 35

#define FIXED_REGISTERS					\
{							\
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,	\
  1, 1, 1						\
}

#define CALL_USED_REGISTERS				\
{							\
  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,	\
  1, 1, 1						\
}

#define CALL_REALLY_USED_REGISTERS			\
{							\
  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1						\
}

/* Internal macros to classify a register number as to whether it's a
   general purpose register, or a status register.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

/*
#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)
#define FP_DBX_FIRST ((write_symbols == DBX_DEBUG) ? 38 : 32)
*/

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Compiler temporary register */
#define TMP_REGNUM (GP_REG_FIRST + 7)

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 33

/* These two registers don't really exist: they get eliminated to either
   the stack or hard frame pointer.  */
#define ARG_POINTER_REGNUM 34
#define FRAME_POINTER_REGNUM GP_REG_FIRST + 30

/* The ABI-defined global pointer.  Sometimes we use a different
   register in leaf functions: see PIC_OFFSET_TABLE_REGNUM.  */
#define GLOBAL_POINTER_REGNUM (GP_REG_FIRST + 28)

/* The following a fake hard registers that describe some of the dedicated
   registers on the FR30.  */
#define CONDITION_CODE_REGNUM 32
#define RETURN_POINTER_REGNUM 31

#define GPR_P(REGNO)   (IN_RANGE_P ((REGNO), GP_REG_FIRST, GP_REG_LAST))

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS,			/* no registers in set */
  GR_REGS,			/* integer registers */
  FP_REGS,			/* floating point registers */
  SP_REG,			/* stack_pointer */
  ALL_REGS,			/* all registers */
  LIM_REG_CLASSES		/* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS GR_REGS

#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "GR_REGS",								\
  "FP_REGS",								\
  "SP_REG",								\
  "ALL_REGS"								\
}

/* An initializer containing the contents of the register classes,
   as integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not
   suffice.  Then the integers are replaced by sub-initializers,
   braced groupings containing several integers.  Each
   sub-initializer must be suitable as an initializer for the type
   `HARD_REG_SET' which is defined in `hard-reg-set.h'.  */

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000 },	/* NO_REGS */				\
  { 0xffffffff, 0x00000000 },	/* GR_REGS */				\
  { 0x00000000, 0x00000000 },	/* FP_REGS */				\
  { 0x00000000, 0x00000002 },	/* SP_REG */				\
  { 0xffffffff, 0xffffffff }	/* ALL_REGS */				\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */
#define REGNO_REG_CLASS(REGNO) 			\
  ( (REGNO) < 32 ? GR_REGS :			\
    ((REGNO) == 33 ? SP_REG			\
     : ALL_REGS))

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */
#define BASE_REG_CLASS  GR_REGS

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */
#define INDEX_REG_CLASS NO_REGS

/* We generally want to put call-clobbered registers ahead of
   call-saved ones.  (IRA expects this.)  */
#define REG_ALLOC_ORDER						\
{								\
  /* Call-clobbered GPRs.  */					\
  7, 0, 1, 2, 3, 4, 5, 6,					\
  /* Return pointer  */						\
  31,								\
  /* Call-saved GPRs.  */					\
  8, 9, 10, 11, 12, 13, 14, 15,					\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,		\
  /* global pointer, base pointer */				\
  29, 30,							\
  /* cc, Stack pointer, arg pointer  */ 			\
  32, 33, 34							\
}

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Return true if a value is inside a range.  */
#define IN_RANGE_P(VALUE, LOW, HIGH)			\
  (((unsigned HOST_WIDE_INT)((VALUE) - (LOW)))		\
   <= ((unsigned HOST_WIDE_INT)((HIGH) - (LOW))))

/* Some range macros.  */
#define INT16_P(X)     ((X) >= - 0x8000 && (X) <= 0x7fff)

/* Stack layout; function entry, exit and calling.  */
#define STACK_GROWS_DOWNWARD
#define FRAME_GROWS_DOWNWARD flag_stack_protect

#define STARTING_FRAME_OFFSET 0

/* Eliminating the Frame Pointer and the Arg Pointer.  */ 

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.

   On some machines, the position of the argument pointer is not known until
   the compilation is completed.  In such a case, a separate hard register must
   be used for the argument pointer.  This register can be eliminated by
   replacing it with either the frame pointer or the argument pointer,
   depending on whether or not the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack pointer is
   specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS				\
{						\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}	\
}

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  do									\
    {									\
      int size = mist32_compute_frame_size (get_frame_size ());		\
      									\
      if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
	(OFFSET) = 0;							\
      else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM) \
	(OFFSET) = size - crtl->args.pretend_args_size;			\
      else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
	(OFFSET) = size - crtl->args.pretend_args_size;			\
      else								\
	gcc_unreachable ();						\
    }									\
  while (0)

/* Allocate stack space for arguments at the beginning of each function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* The argument pointer always points to the first argument.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers.
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in
   `crtl->outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

#define STACK_BOUNDARY 32

/* ALIGN FRAMES on word boundaries */
#define MIST32_STACK_ALIGN(LOC) (((LOC) + 3) & ~ 3)


/* Symbolic macros for the registers used to return integer and floating
   point values.  */
#define GP_RETURN (GP_REG_FIRST + 0)
#define MAX_ARGS_IN_REGISTERS 6

/* Symbolic macros for the first/last argument registers.  */
#define GP_ARG_FIRST (GP_REG_FIRST + 1)
#define GP_ARG_LAST  (GP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)

/*
#define FP_RETURN ((TARGET_SOFT_FLOAT) ? GP_RETURN : (FP_REG_FIRST + 0))
#define FP_ARG_FIRST (FP_REG_FIRST + 12)
#define FP_ARG_LAST  (FP_ARG_FIRST + MAX_ARGS_IN_REGISTERS - 1)
*/

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(N)					\
  ((IN_RANGE((N), GP_ARG_FIRST, GP_ARG_LAST))			\
   && !fixed_regs[N])

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  */
#define CUMULATIVE_ARGS int

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  (CUM) = 0

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory
   to memory in one reasonably fast instruction.  */
#define MOVE_MAX UNITS_PER_WORD

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#ifndef Pmode
#define Pmode SImode
#endif

/* Give call MEMs SImode since it is the "most permissive" mode
   for both 32-bit and 64-bit targets.  */
#define FUNCTION_MODE SImode

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1


/* Control the assembler format that we output.  */
#define REGISTER_NAMES							\
{ "r0",   "r1",   "r2",   "r3",   "r4",     "r5",   "r6",   "rtmp",	\
  "r8",   "r9",   "r10",  "r11",  "r12",    "r13",  "r14",   "r15",	\
  "r16",  "r17",  "r18",  "r19",  "r20",    "r21",  "r22",   "r23",	\
  "r24",  "r25",  "r26",  "r27",  "r28", "rglobl", "rbase", "rret",	\
  "cc",   "stack", "arg"						\
}

/* List the "software" names for each register. */
#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "r7",	 7 + GP_REG_FIRST },					\
  { "r29",	29 + GP_REG_FIRST },					\
  { "r30",	30 + GP_REG_FIRST },					\
  { "r31",	31 + GP_REG_FIRST }					\
}

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)					\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.  */
#define REGNO_OK_FOR_BASE_P(REGNO)			\
  ((REGNO) < FIRST_PSEUDO_REGISTER			\
   ? GPR_P (REGNO) || (REGNO) == ARG_POINTER_REGNUM	\
   : GPR_P (reg_renumber[REGNO]))

/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REGNO_OK_FOR_INDEX_P(REGNO) 0

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */

/* Tie QI/HI/SI modes together.  */
#define MODES_TIEABLE_P(MODE1, MODE2)			\
  (GET_MODE_CLASS (MODE1) == MODE_INT			\
   && GET_MODE_CLASS (MODE2) == MODE_INT		\
   && GET_MODE_SIZE (MODE1) <= UNITS_PER_WORD		\
   && GET_MODE_SIZE (MODE2) <= UNITS_PER_WORD)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl\t"

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(STREAM,LOG)		\
  fprintf (STREAM, "\t.align\t%d\n", LOG)


/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory, i.e., if such access require more than one instruction or if
   there is no difference in cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by finding
   the smallest containing object; when it is defined, a fullword load will be
   used if alignment permits.  Unless bytes accesses are faster than word
   accesses, using word accesses is preferable since it may eliminate
   subsequent memory access if subsequent accesses occur to other fields in the
   same word of the structure, but to different bytes.  */
#define SLOW_BYTE_ACCESS 1


#undef  ASM_SPEC

/* Section selection.  */
#define TEXT_SECTION_ASM_OP	"\t.section\t.text"
#define DATA_SECTION_ASM_OP	"\t.section\t.data"
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

/* How to start an assembler comment.
   The leading space is important (the mist32 native assembler requires it).  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START "#"
#endif

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#ifndef ASM_APP_ON
#define ASM_APP_ON "#APP\n"
#endif

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#ifndef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"
#endif


#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GCC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.  */
#define FUNCTION_PROFILER(FILE, LABELNO)	\
{						\
  fprintf (FILE, "\t push rret\n" );		\
  fprintf (FILE, "\t movepc rret, 8\n" );	\
  fprintf (FILE, "\t lil r0, lo(mcount)\n" );	\
  fprintf (FILE, "\t wr16h r0, hi(mcount)\n" );	\
  fprintf (FILE, "\t b r0\n" );			\
  fprintf (FILE, ".word\tLP%d\n", LABELNO);	\
  fprintf (FILE, "\t pop rret\n" );		\
}

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE 12

/* We want the trampoline to be aligned on a 32bit boundary so that we can
   make sure the location of the static chain & target function within
   the trampoline is also aligned on a 32bit boundary.  */
#define TRAMPOLINE_ALIGNMENT 32

#define INCOMING_RETURN_ADDR_RTX   gen_rtx_REG (Pmode, RETURN_POINTER_REGNUM)

#define MASK_RETURN_ADDR GEN_INT (-3)

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X)		\
  (GET_CODE (X) == LABEL_REF		\
   || GET_CODE (X) == SYMBOL_REF	\
   || CONST_INT_P (X)			\
   || GET_CODE (X) == CONST)

/* Nonzero if the constant value X is a legitimate general operand.
   We don't allow (plus symbol large-constant) as the relocations can't
   describe it.  INTVAL > 32767 handles both 16-bit and 24-bit relocations.
   We allow all CONST_DOUBLE's as the md file patterns will force the
   constant to memory if they can't handle them.  */

#define LEGITIMATE_CONSTANT_P(X)					\
  (! (GET_CODE (X) == CONST						\
      && GET_CODE (XEXP (X, 0)) == PLUS					\
      && (GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF || GET_CODE (XEXP (XEXP (X, 0), 0)) == LABEL_REF) \
      && CONST_INT_P (XEXP (XEXP (X, 0), 1))				\
      && (unsigned HOST_WIDE_INT) INTVAL (XEXP (XEXP (X, 0), 1)) > 0x3ff))

/* mist32 function types.  */
enum mist32_function_type
{
  MIST32_FUNCTION_UNKNOWN, MIST32_FUNCTION_NORMAL, MIST32_FUNCTION_INTERRUPT
};

#define MIST32_INTERRUPT_P(TYPE) ((TYPE) == MIST32_FUNCTION_INTERRUPT)

#endif /* GCC_MIST32_H */
