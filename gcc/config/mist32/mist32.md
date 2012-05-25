;; -*- Scheme -*-
;; Machine description of the mist32 cpu for GNU C compiler
;; Copyright (C) 2012 Free Software Foundation, Inc.
;; Contributed by Hirotaka Kawata <hirotaka@techno-st.net>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; UNSPEC_VOLATILE usage
(define_constants
  [(UNSPECV_BLOCKAGE		0)
   (UNSPECV_FLUSH_ICACHE	1)])

;; UNSPEC usage
(define_constants
  [(UNSPEC_LOAD_SDA_BASE	2)
   (UNSPEC_SET_CBIT		3)
   (UNSPEC_PIC_LOAD_ADDR	4)
   (UNSPEC_GET_PC		5)
   (UNSPEC_GOTOFF		6)
   ])

;; Registers
(define_constants
  [(RETURN_POINTER_REGNUM	31)
   (STACK_POINTER_REGNUM	33)]
)

;;{{{ Attributes 

;; Insn type.  Used to default other attribute values.
(define_attr "type"
  "load,store,arith,fp,branch,move"
  (const_string "arith"))

;; Length in bytes.
(define_attr "length" "" (const_int 4))

;;}}} 

(include "predicates.md")
(include "constraints.md")

;; Expand prologue as RTL
(define_expand "prologue"
  [(const_int 1)]
  ""
  "
{
  mist32_expand_prologue ();
  DONE;
}")

;; Expand epilogue as RTL
(define_expand "epilogue"
  [(return)]
  ""
  "
{
  mist32_expand_epilogue ();
  emit_jump_insn (gen_return ());
  DONE;
}")

;;{{{ Moves 

;; Move instructions.
;;
;; For QI and HI moves, the register must contain the full properly
;; sign-extended value.  nonzero_bits assumes this [otherwise
;; SHORT_IMMEDIATES_SIGN_EXTEND must be used, but the comment for it
;; says it's a kludge and the .md files should be fixed instead].

;;{{{ Push and Pop

(define_insn "pushsi1"
  [(set (mem:SI (pre_dec:SI (reg:SI STACK_POINTER_REGNUM)))
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "push %0")

(define_insn "popsi1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (post_inc:SI (reg:SI STACK_POINTER_REGNUM))))]
  ""
  "pop %0")

(define_expand "add_stack_pointer"
  [(set (reg:SI STACK_POINTER_REGNUM)
	(plus:SI (reg:SI STACK_POINTER_REGNUM)
		 (match_operand:SI 0 "nonmemory_operand" "")))]
  ""
  "
{
  rtx tmp = gen_rtx_REG (Pmode, TMP_REGNUM);

  emit_insn (gen_load_stack_pointer (tmp));
  emit_insn (gen_addsi3 (tmp, tmp, operands[0]));
  emit_insn (gen_save_stack_pointer (tmp));
  DONE;
}")

(define_expand "sub_stack_pointer"
  [(set (reg:SI STACK_POINTER_REGNUM)
	(minus:SI (reg:SI STACK_POINTER_REGNUM)
		  (match_operand:SI 0 "nonmemory_operand" "")))]
  ""
  "
{
  rtx tmp = gen_rtx_REG (Pmode, TMP_REGNUM);

  emit_insn (gen_load_stack_pointer (tmp));
  emit_insn (gen_subsi3 (tmp, tmp, operands[0]));
  emit_insn (gen_save_stack_pointer (tmp));
  DONE;
}")

;;}}}
;;{{{ 1 Byte Moves 

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* Everything except mem = const or mem = mem can be done easily.
     Objects in the small data area are handled too.  */

  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}
")

(define_insn "*loadqi_mem"
  [(set (match_operand:QI 0         "register_operand" "=r")
	(mem:QI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "ld8 %0,%1"
)

(define_insn "*storeqi_mem"
  [(set (mem:QI (match_operand:SI 0 "register_operand" "r"))
	(match_operand:QI 1         "register_operand" "r"))]
  ""
  "st8 %1,%0"
)

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,mT")
	(match_operand:QI 1 "general_operand"       "r,J,mT,r"))]
  "register_operand (operands[0], QImode) || register_operand (operands[1], QImode)"
  "@
   move %0,%1
   lil  %0,%1
   ld8  %0,%1
   st8  %1,%0"
)

;;}}}
;;{{{ 2 Byte Moves 

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* Everything except mem = const or mem = mem can be done easily.  */

  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}
")

(define_insn "*loadhi_mem"
  [(set (match_operand:HI 0         "register_operand" "=r")
	(mem:HI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "ld16 %0,%1"
)

(define_insn "*storehi_mem"
  [(set (mem:HI (match_operand:SI 0 "register_operand" "r"))
	(match_operand:HI 1         "register_operand" "r"))]
  ""
  "st16 %1,%0"
)

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,mT")
	(match_operand:HI 1 "general_operand"       "r,J,K,mT,r"))]
  "register_operand (operands[0], HImode) || register_operand (operands[1], HImode)"
  "@
   move  %0,%1
   lil   %0,%1
   ulil  %0,%1
   ld16  %0,%1
   st16  %1,%0"
)

;;}}}
;;{{{ 4 Byte Moves 

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  /* Everything except mem = const or mem = mem can be done easily.  */

  if (MEM_P (operands[0]))
    operands[1] = force_reg (SImode, operands[1]);

  if (GET_CODE(operands[1]) == MEM &&
      (GET_CODE(XEXP(operands[1], 0)) == LABEL_REF || 
       GET_CODE(XEXP(operands[1], 0)) == SYMBOL_REF)) {
    emit_insn(gen_movsi_split(operands[0], operands[1]));
    DONE;
  }

  if (REG_P (operands[0]) && lih_wl16_operand (operands[1], SImode)) {
    emit_insn(gen_movsi_split(operands[0], operands[1]));
    DONE;
  }
}
")

(define_insn "load_stack_pointer"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(reg:SI STACK_POINTER_REGNUM))]
  ""
  "srspr %0"
)
(define_insn "save_stack_pointer"
  [(set (reg:SI STACK_POINTER_REGNUM)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "srspw %0"
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "lih_wl16_operand" ""))]
  ""
  [(set (match_dup 0)
	(high:SI (match_dup 1)))
   (set (match_dup 0)
	(lo_sum:SI (match_dup 0)
		   (match_dup 1)))]
  ""
)

(define_expand "movsi_split"
  [(set (match_dup 2)
	(high:SI (match_operand:SI 1 "lih_wl16_operand" "")))
   (set (match_operand:SI 0 "register_operand" "")
	(lo_sum:SI (match_dup 2) (match_dup 1)))]
  ""
  "
{
  if (reload_in_progress || reload_completed)
    operands[2] = operands[0];
  else
    operands[2] = gen_reg_rtx (SImode);
}
")

(define_insn "set_hi_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "lih_wl16_operand" "")))]
  ""
  "lih %0,hi(%1)"
)

(define_insn "lo_sum_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "lih_wl16_operand" "")))]
  ""
  "wl16 %0,lo(%2)"
)

(define_insn "*loadsi_mem"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(mem:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "ld32 %0,%1"
)

(define_insn "*storesi_mem"
  [(set (mem:SI (match_operand:SI 0 "register_operand" "r"))
	(match_operand:SI         1 "register_operand" "r"))]
  ""
  "st32 %1,%0"
)

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,mT")
	(match_operand:SI 1 "general_operand"       "r,J,K,L,mT,r"))]
  "register_operand (operands[0], SImode) || register_operand (operands[1], SImode)"
  "@
   move %0,%1
   lil  %0,lo(%1)
   ulil %0,lo(%1)
   lih  %0,hi(%1)
   ld32 %0,%1
   st32 %1,%0"
)

;;}}}
;;{{{ 8 Byte Moves
;;}}}
;;}}}
;;{{{ Load & Store Multiple Registers
;;}}}
;;{{{ Floating Point Moves
;;}}} 

;;{{{ Conversions 

;; Unsigned conversions from a smaller integer to a larger integer

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0,mT")))]
  ""
  "@
   wh16 %0,0x0000
   ld8  %0,%1"
)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "wh16 %0,0x0000"
)

;; Signed conversions from a smaller integer to a larger integer

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"                "=r,r,mT")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,mT,r")))]
  ""
  "@
   sext8 %0,%1
   ld8   %0,%1
   st8   %1,%0"
)

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"                "=r,r,mT")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,mT,r")))]
  ""
  "@
   sext16 %0,%1
   ld16   %0,%1
   st16   %1,%0"
)

;;}}} 
;;{{{ Arithmetic
;;{{{ Addition 

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   add %0,%2
   add %0,%2"
)

(define_insn "increment"
  [(set (match_operand:SI 0 "register_operand"            "=r")
	(pre_inc:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "inc %0,%1"
)

(define_insn "decrement"
  [(set (match_operand:SI 0 "register_operand"            "=r")
	(pre_dec:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "dec %0,%1"
)

;;}}}
;;{{{ Subtraction 

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r")
	(minus:SI (match_operand:SI 1 "register_operand" "0,0")
		  (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   sub %0,%2
   sub %0,%2"
)

;;}}}
;;{{{ Multiplication 

;; Signed multiplication producing 32-bit result from 16-bit inputs
(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand"                          "=r,r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0,0"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand"  "r,I"))))]
  ""
  "@
   mull %0,%2
   mull %0,%2"
)

;; Signed multiplication producing 32-bit result from 32-bit inputs
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "register_operand"  "r,I")))]
  ""
  "@
   mull %0,%2
   mull %0,%2"
)

;;}}}
;;{{{ Division

;; Signed division
(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand"        "=r,r")
	(div:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "register_operand" "r,I")))]
  ""
  "@
   div %0,%2
   div %0,%2"
)

;; Unigned division
(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r")
	(udiv:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "register_operand" "r,I")))]
  ""
  "@
   udiv %0,%2
   udiv %0,%2"
)

;; Signed modulo operation
(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand"        "=r,r")
	(mod:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "register_operand" "r,I")))]
  ""
  "@
   mod %0,%2
   mod %0,%2"
)

;; Unsigned modulo operation
(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r")
	(umod:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "register_operand" "r,I")))]
  ""
  "@
   umod %0,%2
   umod %0,%2"
)

;;}}}
;;}}} 
;;{{{ Shifts 

;; Arithmetic Shift Left
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   shl %0,%2
   shl %0,%2"
)

;; Arithmetic Shift Right
(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "0,0")
		     (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   sar %0,%2
   sar %0,%2"
)

;; Logical Shift Right
(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "0,0")
		     (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   shr %0,%2
   shr %0,%2"
)

;;}}} 
;;{{{ Logical Operations 

;; Logical AND, 32-bit integers
(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "nonmemory_operand" "r")))]
  ""
  "and %0,%1"
)

;; Inclusive OR, 32-bit integers
(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "nonmemory_operand" "r")))]
  ""
  "or %1, %0"
)

;; Exclusive OR, 32-bit integers
(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "nonmemory_operand" "r")))]
  ""
  "xor %1, %0"
)

;; Negative (Zero's comlement), 32-bit integers
(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "neg %0"
)

;; One's complement (Logical Not), 32-bit integers
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(not:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "not %0"
)

;;}}} 
;;{{{ Comparisons 

;; The actual comparisons, generated by the cbranch and/or cstore expanders

(define_insn "*cmpsi_internal"
  [(set (reg:CC 32)
	(compare:CC (match_operand:SI 0 "register_operand"  "r,r")
		    (match_operand:SI 1 "nonmemory_operand" "r,I")))]
  ""
  "@
   cmp %1, %0
   cmp %1, %0"
)

;;}}} 
;;{{{ Branches 

;; Define_expands called by the machine independent part of the compiler
;; to allocate a new comparison register

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
				      [(match_operand:SI 1 "register_operand"  "")
				       (match_operand:SI 2 "nonmemory_operand" "")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))
   (clobber (reg:CC 32))]
  ""
  "operands[1] = force_reg (Pmode, operands[1]);
   operands[2] = force_reg (Pmode, operands[2]);"
)

(define_insn "*branch_true"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(match_operand:SI 1 "register_operand"  "r")
				       (match_operand:SI 2 "nonmemory_operand" "r")])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "cmp %1, %2\n\tbr %3, #%b0"
  [(set (attr "length") (const_int 8))]
)


  ; evalution length attr first, check if label distance from PC.
  ; after, generate branch insn using length_attr.
;{
;  if (get_attr_length (insn) <= 4)
;    return "br %1, #%b0";
;  else
;    {
;     operands[1] = force_reg (Pmode, operands[1]);
;      return "br %1, #%b0";
;    }
;} 
  ; We use 130000/260000 instead of 131072/262144 [(16 << 2) bits] 
  ; to account for slot filling
  ; which is complex to track and inaccurate length specs.
;  [(set (attr "length") (if_then_else (ltu (plus (minus (match_dup 1) (pc))
;						 (const_int 130000))
;					   (const_int 260000))
;				      (const_int 4)
;				      (const_int 12)))]

;;}}} 
;;{{{ Calls & Jumps 

;; Subroutine call instruction returning no value.  Operand 0 is the function
;; to call; operand 1 is the number of bytes of arguments pushed (in mode
;; `SImode', except it is normally a `const_int'); operand 2 is the number of
;; registers used as operands.

(define_expand "call"
  [(set (reg:SI RETURN_POINTER_REGNUM) 
	(plus:SI (pc) (const_int 8)))
   (call (match_operand:SI 0 "call_operand" "")
	 (match_operand 1 "" ""))]
  ""
  "
{
  if (flag_pic)
    crtl->uses_pic_offset_table = 1;
}
")

(define_insn "*call_label"
  [(call (mem:SI (match_operand:SI 0 "call_address_operand" ""))
	 (match_operand 1 "" ""))]
  ""
  "*
{
  if (lih_wl16_operand (operands[0], FUNCTION_MODE))
    return \"lih rtmp,hi(%0)\n\twl16 rtmp,lo(%0)\n\tb %0,#al\";
  else
    return \"b %0,#al\";
}
")

(define_insn "*call_reg"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" ""))]
  ""
  "b %0, #al \t; call_reg"
)

;; Subroutine call instruction returning a value.  Operand 0 is the hard
;; register in which the value is returned.  There are three more operands, the
;; same as the three operands of the `call' instruction (but with numbers
;; increased by one).

;; Subroutines that return `BLKmode' objects use the `call' insn.

(define_expand "call_value"
  [(set (reg:SI RETURN_POINTER_REGNUM)
	(plus:SI (pc) (const_int 8)))
   (set (match_operand 0 "register_operand"  "=r")
	(call (match_operand:SI 1 "call_operand" "")
	      (match_operand 2 "" "")))]
  ""
  "
{
  if (flag_pic)
    crtl->uses_pic_offset_table = 1;
}
")

(define_insn "*call_value_internal_reg"
  [(set (match_operand 0 "register_operand"  "=r")
	(call (mem:SI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "")))]
  ""
  "b %1, #al \t; call"
)

(define_insn "*call_value_internal_label"
  [(set (match_operand 0 "register_operand"  "=r")
	(call (mem:SI (match_operand:SI 1 "call_address_operand" ""))
	      (match_operand 2 "" "")))]
  ""
  "*
{
  if (lih_wl16_operand (operands[1], FUNCTION_MODE))
    return \"lih rtmp,hi(%1)\n\twl16 rtmp,lo(%1)\n\tb %1,#al\";
  else
    return \"b %1,#al\";
}
")


(define_insn "*movpc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (pc)
		 (match_operand 1 "immediate_operand" "I")))]
  ""
  "movepc %0, %1"
)

;; Normal unconditional jump.
;; For a description of the computation of the length 
;; attribute see the branch patterns above.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
{
  if (get_attr_length (insn) <= 4)
    return "br %0, #al";
  else
    {
      operands[0] = force_reg (Pmode, operands[0]);
      return "br %0, #al";
    }
} 
  ; We use 130000/260000 instead of 131072/262144 [(16 << 2) bits] 
  ; to account for slot filling
  ; which is complex to track and inaccurate length specs.
;  [(set (attr "length") (if_then_else (ltu (plus (minus (match_dup 1) (pc))
;						 (const_int 130000))
;					   (const_int 260000))
;				      (const_int 4)
;				      (const_int 12)))]
)

;; Indirect jump through a register
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
  "GET_CODE (operands[0]) != MEM || GET_CODE (XEXP (operands[0], 0)) != PLUS"
  "b %0, #al"
)

(define_insn "return_rret"
  [(parallel [(return) (use (reg:SI RETURN_POINTER_REGNUM))])]
  ""
  "b rret, #al"
)

(define_expand "return"
  [(return)]
  ""
  "
{
  emit_jump_insn (gen_return_rret ());
  DONE;
}
")

;;}}} 

;; PIC

/* When generating pic, we need to load the symbol offset into a register.
   So that the optimizer does not confuse this with a normal symbol load
   we use an unspec.  The offset will be loaded from a constant pool entry,
   since that is the only type of relocation we can use.  */

(define_insn "pic_load_addr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand 1 "" "")] UNSPEC_PIC_LOAD_ADDR))]
  "flag_pic"
  "lil %0, %1"
)

(define_insn "gotoff_load_addr"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand 1 "" "")] UNSPEC_GOTOFF))]
  "flag_pic"
  "lih %0, hi(%1@GOTOFF)\;wl16 %0, lo(%1@GOTOFF)"
  [(set_attr "length"	"8")]
)


;;{{{ Miscellaneous 

;; No operation, needed in case the user uses -g but not -O.
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
)

;; Pseudo instruction that prevents the scheduler from moving code above this
;; point.
(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  "")
;;}}} 
