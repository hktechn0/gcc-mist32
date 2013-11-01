/* Target Code for mist32
   Copyright (C) 2012, 2013 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "expr.h"
#include "function.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "ggc.h"
#include "integrate.h"
#include "df.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "tm-constrs.h"
#include "opts.h"

/* function value */

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE mist32_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE mist32_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P mist32_function_value_regno_p

static rtx
mist32_function_value (const_tree valtype,
		       const_tree fntype_or_decli ATTRIBUTE_UNUSED,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), GP_RETURN);
}

static rtx
mist32_libcall_value (enum machine_mode mode,
		      const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, GP_RETURN);
}

static bool
mist32_function_value_regno_p (const unsigned int regno)
{
  return (regno == GP_RETURN);
}


/* print operand */

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND mist32_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS mist32_print_operand_address

void
mist32_print_operand (FILE *file, rtx x, int code)
{
  rtx x0;

  switch (code)
    {
    case 'b':
      /* Convert GCC's comparison operators into mist32 comparison codes.  */
      switch (GET_CODE (x))
	{
	case EQ:  fprintf (file, "eq"); break;
	case NE:  fprintf (file, "ne"); break;
	case LT:  fprintf (file, "lt"); break;
	case LE:  fprintf (file, "le"); break;
	case GT:  fprintf (file, "gt"); break;
	case GE:  fprintf (file, "ge"); break;
	case LTU: fprintf (file, "ltu"); break;
	case LEU: fprintf (file, "leu"); break;
	case GTU: fprintf (file, "gtu"); break;
	case GEU: fprintf (file, "geu");  break;
	default:
	  output_operand_lossage ("mist32_print_operand: unrecognized %%b code");
	  break;
	}
      return;
      
    case 'B':
      /* Convert GCC's comparison operators into the complimentary mist32
	 comparison codes.  */
      switch (GET_CODE (x))
	{
	case EQ:  fprintf (file, "ne"); break;
	case NE:  fprintf (file, "eq"); break;
	case LT:  fprintf (file, "ge"); break;
	case LE:  fprintf (file, "gt"); break;
	case GT:  fprintf (file, "le"); break;
	case GE:  fprintf (file, "lt"); break;
	case LTU: fprintf (file, "geu"); break;
	case LEU: fprintf (file, "gtu"); break;
	case GTU: fprintf (file, "leu"); break;
	case GEU: fprintf (file, "ltu"); break;
	default:
	  output_operand_lossage ("mist32_print_operand: unrecognized %%B code");
	  break;
	}
      return;

    case 'A':
      /* Print a signed byte value as an unsigned value.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("mist32_print_operand: invalid operand to %%A code");
      else
	{
	  HOST_WIDE_INT val;
	  
	  val = INTVAL (x);
	  val &= 0xff;

	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, val);
	}
      return;
      
    case 'x':
      if (GET_CODE (x) != CONST_INT
	  || INTVAL (x) < 16
	  || INTVAL (x) > 32)
	output_operand_lossage ("mist32_print_operand: invalid %%x code");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) - 16);
      return;

    case 'F':
      if (GET_CODE (x) != CONST_DOUBLE)
	output_operand_lossage ("mist32_print_operand: invalid %%F code");
      else
	{
	  char str[30];

	  real_to_decimal (str, CONST_DOUBLE_REAL_VALUE (x),
			   sizeof (str), 0, 1);
	  fputs (str, file);
	}
      return;
      
    case 0:
      /* Handled below.  */
      break;
      
    default:
      fprintf (stderr, "unknown code = %x\n", code);
      output_operand_lossage ("mist32_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      x0 = XEXP (x,0);
      
      switch (GET_CODE (x0))
	{
	case REG:
	  gcc_assert ((unsigned) REGNO (x0) < ARRAY_SIZE (reg_names));
	  fprintf (file, "%s", reg_names [REGNO (x0)]);
	  break;
	  
	case SYMBOL_REF:
	  output_address (x0);
	  break;
	  
	default:
	  fprintf (stderr, "bad MEM code = %x\n", GET_CODE (x0));
	  debug_rtx (x);
	  output_operand_lossage ("mist32_print_operand: unhandled MEM");
	  break;
	}
      break;
      
    case CONST_DOUBLE :
      /* We handle SFmode constants here as output_addr_const doesn't.  */
      if (GET_MODE (x) == SFmode)
	{
	  REAL_VALUE_TYPE d;
	  long l;

	  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	  REAL_VALUE_TO_TARGET_SINGLE (d, l);
	  fprintf (file, "0x%08lx", l);
	  break;
	}

      /* Fall through.  Let output_addr_const deal with it.  */
    default:
      output_addr_const (file, x);
      break;
    }

  return;
}

void
mist32_print_operand_address (FILE *stream, rtx address)
{
  switch (GET_CODE (address))
    {
    case SYMBOL_REF:
      output_addr_const (stream, address);
      break;
      
    default:
      fprintf (stderr, "code = %x\n", GET_CODE (address));
      debug_rtx (address);
      output_operand_lossage ("mist32_print_operand_address: unhandled address");
      break;
    }
}


#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P mist32_legitimate_address_p
/*
#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P mist32_legitimate_constant_p
*/

static bool
mist32_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  if (! REG_P (x))
    return false;
  
  if (strict)
    {
      if (GPR_P (x))
	return true;
    }
  else
    {
      if (GPR_P (x)
	  || REGNO (x) == ARG_POINTER_REGNUM
	  || ! HARD_REGISTER_P (x))
	return true;
    }

  return false;
}

/*
static bool
mist32_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return !(GET_CODE (x) == CONST
	   && GET_CODE (XEXP (x, 0)) == PLUS
	   && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && UINTVAL (XEXP (XEXP (x, 0), 1)) > 0x3ff);
}
*/

/*
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST m32r_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS m32r_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_bool_0

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY m32r_return_in_memory

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE mist32_conditional_register_usage
*/



/* Structure to be filled in by mist32_compute_frame_size() with register
   save masks, and offsets for the current function.  */
struct mist32_frame_info
{
  unsigned int total_size;	/* # Bytes that the entire frame takes up.  */
  unsigned int extra_size;	/* # bytes of extra stuff.  */
  unsigned int pretend_size;	/* # Bytes we push and pretend caller did.  */
  unsigned int args_size;	/* # Bytes that outgoing arguments take up.  */
  unsigned int reg_size;	/* # Bytes needed to store regs.  */
  unsigned int var_size;	/* # Bytes that variables take up.  */
  unsigned int frame_size;      /* # Bytes in current frame.  */
  unsigned int gmask;		/* Mask of saved registers.  */
  unsigned int save_fp;		/* Nonzero if frame pointer must be saved.  */
  unsigned int save_rp;		/* Nonzero if return pointer must be saved.  */
  int          initialized;	/* Nonzero if frame size already calculated.  */
};

/* Current frame information calculated by mist32_compute_frame_size().  */
static struct mist32_frame_info 	current_frame_info;

/* Zero structure to initialize current_frame_info.  */
static struct mist32_frame_info 	zero_frame_info;

#define FRAME_POINTER_MASK 	(1 << (FRAME_POINTER_REGNUM))
#define RETURN_POINTER_MASK 	(1 << (RETURN_POINTER_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.  */
#define MUST_SAVE_REGISTER(regno, interrupt_p)				\
  ((regno) != RETURN_POINTER_REGNUM && (regno) != FRAME_POINTER_REGNUM	\
   && ((df_regs_ever_live_p (regno) && !call_really_used_regs[regno])	\
       || (interrupt_p && call_really_used_regs[regno])))

#define MUST_SAVE_FRAME_POINTER	 (df_regs_ever_live_p (FRAME_POINTER_REGNUM))
#define MUST_SAVE_RETURN_POINTER (df_regs_ever_live_p (RETURN_POINTER_REGNUM) || crtl->profile)

/* The value of TARGET_ATTRIBUTE_TABLE.  */
static const struct attribute_spec mist32_attribute_table[] = {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "interrupt", 0, 0, true,  false, false, NULL },
  { NULL,        0, 0, false, false, false, NULL }
};

/* Initialize the GCC target structure.  */
#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE mist32_attribute_table

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE)				\
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE)				\
  ((MODE) == BLKmode						\
   ? ROUND_ADVANCE ((unsigned int) int_size_in_bytes (TYPE))	\
   : ROUND_ADVANCE ((unsigned int) GET_MODE_SIZE (MODE)))

/* Round CUM up to the necessary point for argument MODE/TYPE.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) (CUM)

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   This macro is only used in this file.  */
#define PASS_IN_REG_P(CUM, MODE, TYPE) \
  (ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)) < MAX_ARGS_IN_REGISTERS)

/* Forward declaration.  */
static void mist32_setup_incoming_varargs (CUMULATIVE_ARGS *, enum machine_mode,
					   tree, int *, int);
static rtx mist32_function_arg (CUMULATIVE_ARGS *, enum machine_mode,
				const_tree, bool);
static void mist32_function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode,
					 const_tree, bool);

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS mist32_setup_incoming_varargs
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK mist32_must_pass_in_stack
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE hook_pass_by_reference_must_pass_in_stack
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES mist32_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG mist32_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE mist32_function_arg_advance

/* Return true if we should pass an argument on the stack rather than
   in registers.  */
static bool
mist32_must_pass_in_stack (enum machine_mode mode, const_tree type)
{
  if (mode == BLKmode)
    return true;
  if (type == NULL)
    return false;
  return AGGREGATE_TYPE_P (type);
}

/* Returns the number of bytes in which *part* of a parameter of machine
   mode MODE and tree type TYPE (which may be NULL if the type is not known).
   If the argument fits entirely in the argument registers, or entirely on
   the stack, then 0 is returned.
   CUM is the number of argument registers already used by earlier
   parameters to the function.  */

static int
mist32_arg_partial_bytes (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			  tree type, bool named ATTRIBUTE_UNUSED)
{
  int words;
  unsigned int size =
    (((mode == BLKmode && type)
      ? (unsigned int) int_size_in_bytes (type)
      : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
    / UNITS_PER_WORD;

  if (*cum >= MAX_ARGS_IN_REGISTERS)
    words = 0;
  else if (*cum + size > MAX_ARGS_IN_REGISTERS)
    words = (*cum + size) - MAX_ARGS_IN_REGISTERS;
  else
    words = 0;

  return words * UNITS_PER_WORD;
}

static void
mist32_setup_incoming_varargs (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			       tree type, int *pretend_size, int no_rtl)
{
  int first_anon_arg;

  if (no_rtl)
    return;

  /* All BLKmode values are passed by reference.  */
  gcc_assert (mode != BLKmode);

  first_anon_arg = (ROUND_ADVANCE_CUM (*cum, mode, type)
		    + ROUND_ADVANCE_ARG (mode, type));

  if (first_anon_arg < MAX_ARGS_IN_REGISTERS)
    {
      /* Note that first_reg_offset < MAX_ARGS_IN_REGISTERS.  */
      int first_reg_offset = GP_ARG_FIRST + first_anon_arg;
      /* Size in words to "pretend" allocate.  */
      int size = MAX_ARGS_IN_REGISTERS - first_anon_arg;
      rtx regblock;

      regblock = gen_frame_mem (BLKmode,
				plus_constant (arg_pointer_rtx,
					       FIRST_PARM_OFFSET (0)));
      set_mem_alias_set (regblock, get_varargs_alias_set ());
      move_block_from_reg (first_reg_offset, regblock, size);

      *pretend_size = (size * UNITS_PER_WORD);
    }
}

static rtx
mist32_function_arg (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		   const_tree type ATTRIBUTE_UNUSED,
		   bool named ATTRIBUTE_UNUSED)
{
  return (PASS_IN_REG_P (*cum, mode, type)
	  ? gen_rtx_REG (mode, GP_ARG_FIRST + 
			 ROUND_ADVANCE_CUM (*cum, mode, type))
	  : NULL_RTX);
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

static void
mist32_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  *cum = (ROUND_ADVANCE_CUM (*cum, mode, type)
	  + ROUND_ADVANCE_ARG (mode, type));
}

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE mist32_can_eliminate

/* Worker function for TARGET_CAN_ELIMINATE.  */

bool
mist32_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM
          ? ! frame_pointer_needed
          : true);
}

/* Type of function DECL.

   The result is cached.  To reset the cache at the end of a function,
   call with DECL = NULL_TREE.  */

enum mist32_function_type
mist32_compute_function_type (tree decl)
{
  /* Cached value.  */
  static enum mist32_function_type fn_type = MIST32_FUNCTION_UNKNOWN;
  /* Last function we were called for.  */
  static tree last_fn = NULL_TREE;

  /* Resetting the cached value?  */
  if (decl == NULL_TREE)
    {
      fn_type = MIST32_FUNCTION_UNKNOWN;
      last_fn = NULL_TREE;
      return fn_type;
    }

  if (decl == last_fn && fn_type != MIST32_FUNCTION_UNKNOWN)
    return fn_type;

  /* Compute function type.  */
  fn_type = (lookup_attribute ("interrupt", DECL_ATTRIBUTES (current_function_decl)) != NULL_TREE
	     ? MIST32_FUNCTION_INTERRUPT
	     : MIST32_FUNCTION_NORMAL);

  last_fn = decl;
  return fn_type;
}

/* Returns the number of bytes offset between FROM_REG and TO_REG
   for the current function.  As a side effect it fills in the 
   current_frame_info structure, if the data is available.  */
unsigned int
mist32_compute_frame_size (int size)	/* # of var. bytes allocated.  */
{
  unsigned int regno;
  unsigned int total_size, var_size, args_size, pretend_size, extra_size;
  unsigned int reg_size;
  unsigned int gmask;
  enum mist32_function_type fn_type;
  int interrupt_p;

  var_size	= MIST32_STACK_ALIGN (size);
  args_size	= MIST32_STACK_ALIGN (crtl->outgoing_args_size);
  pretend_size	= crtl->args.pretend_args_size;
  extra_size	= FIRST_PARM_OFFSET (0);
  total_size	= extra_size + pretend_size + args_size + var_size;
  reg_size	= 0;
  gmask		= 0;

  /* See if this is an interrupt handler.  Call used registers must be saved
     for them too.  */
  fn_type = mist32_compute_function_type (current_function_decl);
  interrupt_p = MIST32_INTERRUPT_P (fn_type);

  /* Calculate space needed for registers.  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if (MUST_SAVE_REGISTER (regno, interrupt_p))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1 << regno;
	}
    }

  current_frame_info.save_fp = MUST_SAVE_FRAME_POINTER;
  current_frame_info.save_rp = MUST_SAVE_RETURN_POINTER;

  reg_size += ((current_frame_info.save_fp + current_frame_info.save_rp)
	       * UNITS_PER_WORD);
  total_size += reg_size;

  /* ??? Not sure this is necessary, and I don't think the epilogue
     handler will do the right thing if this changes total_size.  */
  total_size = MIST32_STACK_ALIGN (total_size);

  /* frame_size = total_size - (pretend_size + reg_size); */

  /* Save computed information.  */
  current_frame_info.total_size   = total_size;
  current_frame_info.extra_size   = extra_size;
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.var_size     = var_size;
  current_frame_info.args_size    = args_size;
  current_frame_info.reg_size	  = reg_size;
  current_frame_info.gmask	  = gmask;
  current_frame_info.initialized  = reload_completed;

  /* Ok, we're done.  */
  return total_size;
}

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE mist32_output_function_prologue
#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE mist32_output_function_epilogue

/* Expand the mist32 prologue as a series of insns.  */

void
mist32_expand_prologue (void)
{
  int regno;
  int frame_size;
  unsigned int gmask;

  if (! current_frame_info.initialized)
    mist32_compute_frame_size (get_frame_size ());

  gmask = current_frame_info.gmask;

  /* These cases shouldn't happen.  Catch them now.  */
  gcc_assert (current_frame_info.total_size || !gmask);

  /* Allocate space for register arguments if this is a variadic function.  */
  if (current_frame_info.pretend_size != 0)
    {
      /* Use a HOST_WIDE_INT temporary, since negating an unsigned int gives
	 the wrong result on a 64-bit host.  */
      HOST_WIDE_INT pretend_size = current_frame_info.pretend_size;
      emit_insn (gen_sub_stack_pointer (GEN_INT (pretend_size)));
    }

  /* Save any registers we need to and set up fp.  */
  if (current_frame_info.save_fp)
    emit_insn (gen_pushsi1 (frame_pointer_rtx));

  gmask &= ~(FRAME_POINTER_MASK | RETURN_POINTER_MASK);

  /* Save any needed call-saved regs (and call-used if this is an
     interrupt handler).  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if ((gmask & (1 << regno)) != 0)
	emit_insn (gen_pushsi1 (gen_rtx_REG (Pmode, regno)));
    }

  if (current_frame_info.save_rp)
    emit_insn (gen_pushsi1 (gen_rtx_REG (Pmode, RETURN_POINTER_REGNUM)));

  /* Allocate the stack frame.  */
  frame_size = (current_frame_info.total_size
		- (current_frame_info.pretend_size
		   + current_frame_info.reg_size));

  if (frame_size == 0)
    ; /* Nothing to do.  */
  else if (frame_size <= 0x3ff)
    emit_insn (gen_sub_stack_pointer (GEN_INT (frame_size)));
  else
    {
      int remaining = frame_size;

      while(remaining > 0)
	{
	  emit_insn (gen_sub_stack_pointer (GEN_INT (remaining & 0x3ff)));
	  remaining -= 0x3ff;
	}
    }

  if (frame_pointer_needed)
    emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));

  if (crtl->profile)
    /* Push rret for mcount (form_pc, x).  */
    emit_insn (gen_pushsi1 (gen_rtx_REG (Pmode, RETURN_POINTER_REGNUM)));

  if (crtl->profile)
    emit_insn (gen_blockage ());
}

/* Set up the stack and frame pointer (if desired) for the function.
   Note, if this is changed, you need to mirror the changes in
   mist32_compute_frame_size which calculates the prolog size.  */

static void
mist32_output_function_prologue (FILE * file, HOST_WIDE_INT size)
{
  enum mist32_function_type fn_type = mist32_compute_function_type (current_function_decl);

  /* If this is an interrupt handler, mark it as such.  */
  if (MIST32_INTERRUPT_P (fn_type))
    fprintf (file, "\t%s interrupt handler\n", ASM_COMMENT_START);

  if (! current_frame_info.initialized)
    mist32_compute_frame_size (size);

  /* This is only for the human reader.  */
  fprintf (file,
	   "\t%s PROLOGUE, vars= %d, regs= %d, args= %d, extra= %d\n",
	   ASM_COMMENT_START,
	   current_frame_info.var_size,
	   current_frame_info.reg_size / 4,
	   current_frame_info.args_size,
	   current_frame_info.extra_size);
}

/* Expand the mist32 epilogue as a series of insns.  */

void
mist32_expand_epilogue (void)
{
  int regno;
  int noepilogue = FALSE;
  int total_size;

  gcc_assert (current_frame_info.initialized);
  total_size = current_frame_info.total_size;

  if (total_size == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (insn && NOTE_P (insn))
	insn = prev_nonnote_insn (insn);
      if (insn && BARRIER_P (insn))
	noepilogue = TRUE;
    }

  if (!noepilogue)
    {
      unsigned int var_size = current_frame_info.var_size;
      unsigned int args_size = current_frame_info.args_size;
      unsigned int gmask = current_frame_info.gmask;
      int can_trust_sp_p = !cfun->calls_alloca;

      if (flag_exceptions)
        emit_insn (gen_blockage ());

      /* The first thing to do is point the sp at the bottom of the register
	 save area.  */
      if (can_trust_sp_p || frame_pointer_needed)
	{
	  unsigned int reg_offset = var_size + args_size;

	  if (frame_pointer_needed)
	    emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));

	  if (reg_offset == 0)
	    ; /* Nothing to do.  */
	  else if (reg_offset <= 0x3ff)
	    emit_insn (gen_add_stack_pointer (GEN_INT (reg_offset)));
	  else
	    {
	      int remaining = reg_offset;

	      while(remaining > 0)
		{
		  emit_insn (gen_sub_stack_pointer (GEN_INT (remaining & 0x3ff)));
		  remaining -= 0x3ff;
		}
	    }
	}
      else
	gcc_unreachable ();

      if (current_frame_info.save_rp)
	emit_insn (gen_popsi1 (gen_rtx_REG (Pmode, RETURN_POINTER_REGNUM)));

      gmask &= ~(FRAME_POINTER_MASK | RETURN_POINTER_MASK);

      /* Restore any saved registers, in reverse order of course.  */
      for (regno = GP_REG_LAST; regno >= 0; --regno)
	{
	  if ((gmask & (1 << regno)) != 0)
	    emit_insn (gen_popsi1 (gen_rtx_REG (Pmode, regno)));
	}

      if (current_frame_info.save_fp)
	emit_insn (gen_popsi1 (gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM)));

      /* Remove varargs area if present.  */
      if (current_frame_info.pretend_size != 0)
	emit_insn (gen_add_stack_pointer (GEN_INT (current_frame_info.pretend_size)));

      emit_insn (gen_blockage ());
    }
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */

static void
mist32_output_function_epilogue (FILE * file,
				 HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  /* This is only for the human reader.  */
  fprintf (file, "\t%s EPILOGUE\n", ASM_COMMENT_START);

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;
  mist32_compute_function_type (NULL_TREE);
}

rtx
mist32_return_addr (int count)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RETURN_POINTER_REGNUM);
}

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"

/* Trampoline */

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE mist32_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT mist32_trampoline_init

#undef  TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO sjlj_except_unwind_info

static void
mist32_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\tlih\thi(#0), %s\n", reg_names [29]);
  fprintf (f, "\twl16\tlo(#0), %s\n", reg_names [29]);
  fprintf (f, "\tb\t%s,#AL\n", reg_names [29]);
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
mist32_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, 4);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 12);
  emit_move_insn (mem, fnaddr);
}

struct gcc_target targetm = TARGET_INITIALIZER;
