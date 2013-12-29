;; Machine description for mist32
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


;; -------------------------------------------------------------------------
;; mist32 specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "predicates.md")
(include "constraints.md")

; mist32 instructions are 4 bytes long
(define_attr "length" "" (const_int 4))

; instruction type
(define_attr "type"
  "load,store,arith,fp,branch,move"
  (const_string "arith"))

;; -------------------------------------------------------------------------
;; Define
;; -------------------------------------------------------------------------

; Registers
(define_constants
  [(RETURN_POINTER_REGNUM	31)
   (CONDITION_CODE_REGNUM	32)
   (STACK_POINTER_REGNUM	33)]
)

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
)


;; Expand prologue as RTL
(define_expand "prologue"
  [(const_int 1)]
  ""
  "
{
  mist32_expand_prologue ();
  DONE;
}
")

;; Expand epilogue as RTL
(define_expand "epilogue"
  [(return)]
  ""
  "
{
  mist32_expand_epilogue ();
  emit_jump_insn (gen_returner ());
  DONE;
}
")

;; -------------------------------------------------------------------------
;; push / pop and stack instruction
;; -------------------------------------------------------------------------

(define_insn "pushsi1"
  [(set (mem:SI (pre_dec:SI (reg:SI STACK_POINTER_REGNUM)))
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "push\t%0"
)

(define_insn "popsi1"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (post_inc:SI (reg:SI STACK_POINTER_REGNUM))))]
  ""
  "pop\t%0"
)

; add stack pointer split
(define_split
  [(set (match_operand:SI 0 "stack_pointer_operand" "")
	(plus:SI (match_dup 0)
		 (match_operand:SI 1 "nonmemory_operand" "")))]
  ""
  [(set (match_dup 2) (match_dup 0))
   (set (match_dup 2) (plus:SI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (match_dup 2))]
  "
{ operands[2] = gen_rtx_REG (Pmode, TMP_REGNUM); }
")

; sub stack pointer split
(define_split
  [(set (match_operand:SI 0 "stack_pointer_operand" "")
	(minus:SI (match_dup 0)
		  (match_operand:SI 1 "nonmemory_operand" "")))]
  ""
  [(set (match_dup 2) (match_dup 0))
   (set (match_dup 2) (minus:SI (match_dup 2) (match_dup 1)))
   (set (match_dup 0) (match_dup 2))]
  "
{ operands[2] = gen_rtx_REG (Pmode, TMP_REGNUM); }
")

(define_expand "add_stack_pointer"
  [(set (reg:SI STACK_POINTER_REGNUM)
	(plus:SI (reg:SI STACK_POINTER_REGNUM)
		 (match_operand:SI 0 "nonmemory_operand" "")))]
  ""
  "
{
  if (! o2_i11_operand (operands[0], SImode)) {
    if(! (reload_in_progress || reload_completed))
      operands[0] = force_reg (SImode, operands[0]);
  }
}
")

(define_expand "sub_stack_pointer"
  [(set (reg:SI STACK_POINTER_REGNUM)
	(minus:SI (reg:SI STACK_POINTER_REGNUM)
		  (match_operand:SI 0 "nonmemory_operand" "")))]
  ""
  "
{
  if (! o2_i11_operand (operands[0], SImode)) {
    if(! (reload_in_progress || reload_completed))
      operands[0] = force_reg (SImode, operands[0]);
  }
}
")

;; -------------------------------------------------------------------------
;; Move instruction
;; -------------------------------------------------------------------------

;; QI mode

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}
")

(define_insn "*loadqi_disp"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (plus:SI (match_operand:SI 1 "register_operand" "%r")
			 (match_operand:SI 2 "disp6_operand" "i"))))]
  ""
  "ldd8\t%0, %1, %2"
)

(define_insn "*storeqi_disp"
  [(set (mem:QI (plus:SI (match_operand:SI 0 "register_operand" "%r")
			 (match_operand:SI 1 "disp6_operand" "i")))
	(match_operand:QI 2 "register_operand" "r"))]
  ""
  "std8\t%2, %0, %1"
)

(define_insn "*loadqi_mem"
  [(set (match_operand:QI 0         "register_operand" "=r")
	(mem:QI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "ld8\t%0, %1"
)

(define_insn "*storeqi_mem"
  [(set (mem:QI (match_operand:SI 0 "register_operand" "r"))
	(match_operand:QI 1         "register_operand" "r"))]
  ""
  "st8\t%1, %0"
)

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,mT")
	(match_operand:QI 1 "general_operand"       "r,J,mT,r"))]
  "register_operand (operands[0], QImode) || register_operand (operands[1], QImode)"
  "@
   move\t%0, %1
   lil\t%0, %1
   ld8\t%0, %1
   st8\t%1, %0"
)

;; HI mode

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}
")

(define_insn "*loadhi_disp"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "register_operand" "%r")
			 (match_operand:SI 2 "disp7_operand" "i"))))]
  ""
  "ldd16\t%0, %1, %2"
)

(define_insn "*storehi_disp "
  [(set (mem:HI (plus:SI (match_operand:SI 0 "register_operand" "%r")
			 (match_operand:SI 1 "disp7_operand" "i")))
	(match_operand:HI 2 "register_operand" "r"))]
  ""
  "std16\t%2, %0, %1"
)

(define_insn "*loadhi_mem"
  [(set (match_operand:HI 0         "register_operand" "=r")
	(mem:HI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "ld16\t%0, %1"
)

(define_insn "*storehi_mem"
  [(set (mem:HI (match_operand:SI 0 "register_operand" "r"))
	(match_operand:HI 1         "register_operand" "r"))]
  ""
  "st16\t%1, %0"
)

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,r,mT")
	(match_operand:HI 1 "general_operand"       "r,J,K,mT,r"))]
  "register_operand (operands[0], HImode) || register_operand (operands[1], HImode)"
  "@
   move\t%0, %1
   lil\t%0, %1
   ulil\t%0, %1
   ld16\t%0, %1
   st16\t%1, %0"
)

;; SI mode

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  /* If this is a store, force the value into a register.  */
  if(! (reload_in_progress || reload_completed)) {
    if (MEM_P (operands[0]))
      operands[1] = force_reg (SImode, operands[1]);
  }

  if (MEM_P (operands[1])
      && (GET_CODE (XEXP (operands[1], 0)) == LABEL_REF
          || GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF)) {
    emit_insn(gen_movsi_split(operands[0], operands[1]));
    DONE;
  }

  if (REG_P (operands[0])
      && lih_wl16_operand (operands[1], SImode)) {
    emit_insn(gen_movsi_split(operands[0], operands[1]));
    DONE;
  }
}
")

(define_expand "movsi_split"
  [(set (match_operand:SI 0 "register_operand" "")
	(high:SI (match_operand:SI 1 "lih_wl16_operand" "")))
   (set (match_dup 0)
	(lo_sum:SI (match_dup 0) (match_dup 1)))]
  ""
  ""
)

(define_insn "*movepc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (pc)
		 (match_operand 1 "immediate_operand" "I")))]
  ""
  "movepc\t%0, %1"
)

(define_insn "load_stack_pointer"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(reg:SI STACK_POINTER_REGNUM))]
  ""
  "srspr\t%0"
)

(define_insn "save_stack_pointer"
  [(set (reg:SI STACK_POINTER_REGNUM)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "srspw\t%0"
)

(define_insn "*loadsi_disp"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "%r")
			 (match_operand:SI 2 "disp8_operand" "i"))))]
  ""
  "ldd32\t%0, %1, %2"
)

(define_insn "*storesi_disp"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "register_operand" "%r")
			 (match_operand:SI 1 "disp8_operand" "i")))
	(match_operand:SI 2 "register_operand" "r"))]
  ""
  "std32\t%2, %0, %1"
)

(define_insn "set_hi_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "lih_wl16_operand" "g")))]
  ""
  "lih\t%0, hi(%1)"
)

(define_insn "lo_sum_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "%0")
		   (match_operand:SI 2 "lih_wl16_operand" "g")))]
  ""
  "wl16\t%0, lo(%2)"
)

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(const:SI (plus:SI (match_operand:SI 1 "symbolic_operand" "")
			   (match_operand:SI 2 "const_int_operand" ""))))]
  ""
  [(set (match_dup 0)
	(high:SI (match_dup 1)))
   (set (match_dup 0)
	(lo_sum:SI (match_dup 0)
		   (match_dup 1)))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_dup 2)))]
   ""
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

(define_insn "*loadsi_mem"
  [(set (match_operand:SI         0 "register_operand" "=r")
	(mem:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "ld32\t%0, %1"
)

(define_insn "*storesi_mem"
  [(set (mem:SI (match_operand:SI 0 "register_operand" "r"))
	(match_operand:SI         1 "register_operand" "r"))]
  ""
  "st32\t%1, %0"
)

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,mT,r,k,r")
	(match_operand:SI 1 "general_operand"       "r,J,K,L,mT,r,k,r,R"))]
  "register_operand (operands[0], SImode) || register_operand (operands[1], SImode)"
  "@
   move\t%0, %1
   lil\t%0, %1
   ulil\t%0, %1
   lih\t%0, hi(%1)
   ld32\t%0, %1
   st32\t%1, %0
   srspr\t%0
   srspw\t%1
   #"
)

;; -------------------------------------------------------------------------
;; Conversion instructions
;; -------------------------------------------------------------------------

;; Unsigned conversions

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0,mT")))]
  ""
  "@
   get8\t%0, 0
   ld8\t%0, %1"
)

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                "=r,r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0,mT")))]
  ""
  "@
   wh16\t%0, 0x0000
   ld16\t%0, %1"
)

;; Signed conversions

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"                "=r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r")))]
  ""
  "sext8\t%0, %1"
)

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand"                "=r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r")))]
  ""
  "sext16\t%0, %1"
)

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

;; Addition 

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!o2_i11_operand (operands[2], SImode)) {
    if(! (reload_in_progress || reload_completed))
      operands[2] = force_reg (SImode, operands[2]);
  }
}
")

(define_insn "*inc"
  [(set (match_operand:SI 0 "general_register_operand" "=r")
	(plus:SI (match_operand:SI 1 "general_register_operand" "r")
		 (const_int 1)))]
  ""
  "inc\t%0, %1"
)

(define_insn "*dec_"
  [(set (match_operand:SI 0 "general_register_operand" "=r")
	(plus:SI (match_operand:SI 1 "general_register_operand" "r")
		 (const_int -1)))]
  ""
  "dec\t%0, %1"
)

(define_insn "*addsi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r,k")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,0")
		 (match_operand:SI 2 "o2_i11_operand" "r,I,rI")))]
  ""
  "@
   add\t%0, %2
   add\t%0, %2
   #"
)

;; Subtraction

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 1 "register_operand" "")
		  (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "
{
  if (!o2_i11_operand (operands[2], SImode)) {
    if(! (reload_in_progress || reload_completed))
      operands[2] = force_reg (SImode, operands[2]);
  }
}
")

(define_insn "*dec"
  [(set (match_operand:SI 0 "general_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "general_register_operand" "r")
		  (const_int 1)))]
  ""
  "dec\t%0, %1"
)

(define_insn "*subsi3_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r,k")
	(minus:SI (match_operand:SI 1 "register_operand" "0,0,0")
		  (match_operand:SI 2 "o2_i11_operand" "r,I,rI")))]
  ""
  "@
   sub\t%0, %2
   sub\t%0, %2
   #"
)

;; Multiplication 

;; Signed multiplication producing 32-bit result from 16-bit inputs
(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand"                          "=r,r")
	(mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%0,0"))
		 (sign_extend:SI (match_operand:HI 2 "register_operand"  "r,I"))))]
  ""
  "@
   mull\t%0, %2
   mull\t%0, %2"
)

;; Signed multiplication producing 32-bit result from 32-bit inputs
(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"          "=r,r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "register_operand"  "r,I")))]
  ""
  "@
   mull\t%0, %2
   mull\t%0, %2"
)

;; Division

;; Signed division
(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand"        "=r,r")
	(div:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "register_operand" "r,I")))]
  ""
  "@
   div\t%0, %2
   div\t%0, %2"
)

;; Unigned division
(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r")
	(udiv:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "register_operand" "r,M")))]
  ""
  "@
   udiv\t%0, %2
   udiv\t%0, %2"
)

;; Signed modulo operation
(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand"        "=r,r")
	(mod:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "register_operand" "r,I")))]
  ""
  "@
   mod\t%0, %2
   mod\t%0, %2"
)

;; Unsigned modulo operation
(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r")
	(umod:SI (match_operand:SI 1 "register_operand" "0,0")
		 (match_operand:SI 2 "register_operand" "r,M")))]
  ""
  "@
   umod\t%0, %2
   umod\t%0, %2"
)

;; -------------------------------------------------------------------------
;; Shift operators
;; -------------------------------------------------------------------------

;; Arithmetic Shift Left
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r,r")
	(ashift:SI (match_operand:SI 1 "register_operand"  "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,M")))]
  ""
  "@
   shl\t%0, %2
   shl\t%0, %2"
)

;; Arithmetic Shift Right
(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"  "0,0")
		     (match_operand:SI 2 "nonmemory_operand" "r,M")))]
  ""
  "@
   sar\t%0, %2
   sar\t%0, %2"
)

;; Logical Shift Right
(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"  "0,0")
		     (match_operand:SI 2 "nonmemory_operand" "r,M")))]
  ""
  "@
   shr\t%0, %2
   shr\t%0, %2"
)

;; Rotate Shift Left
(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "register_operand"            "=r,r")
	(rotate:SI (match_operand:SI 1 "register_operand"  "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "r,M")))]
  ""
  "@
   rol\t%0, %2
   rol\t%0, %2"
)

;; Rotate Shift Right
(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand"              "=r,r")
	(rotatert:SI (match_operand:SI 1 "register_operand"  "0,0")
		     (match_operand:SI 2 "nonmemory_operand" "r,M")))]
  ""
  "@
   ror\t%0, %2
   ror\t%0, %2"
)

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

;; Logical AND, 32-bit integers
(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "nonmemory_operand" "r")))]
  ""
  "and\t%0, %2"
)

(define_insn "*nandsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(ior:SI (not:SI (match_operand:SI 1 "register_operand" "%0"))
		(not:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "nand\t%0, %2"
)

;; Inclusive OR, 32-bit integers
(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "nonmemory_operand" "r")))]
  ""
  "or\t%0, %2"
)

(define_insn "*inorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "%0"))
		(not:SI (match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "nor\t%0, %2"
)

;; Exclusive OR, 32-bit integers
(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "nonmemory_operand" "r")))]
  ""
  "xor\t%0, %2"
)

(define_insn "*xnorsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r")
	(not:SI (xor:SI (match_operand:SI 1 "register_operand" "%0")
			(match_operand:SI 2 "nonmemory_operand" "r"))))]
  ""
  "xnor\t%0, %2"
)

;; Negative (Zero's comlement), 32-bit integers
(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "neg\t%0, %1"
)

;; One's complement (Logical Not), 32-bit integers
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand"        "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "not\t%0, %1"
)

;; Byte Swap
(define_insn "bswapsi2"
  [(set (match_operand:SI 0 "register_operand"          "=r")
	(bswap:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "rev8\t%0, %1"
)

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

;; The actual comparisons, generated by the cbranch

(define_insn "*cmpsi"
  [(set (reg:CC 32)
	(compare:CC (match_operand:SI 0 "register_operand"  "r,r")
		    (match_operand:SI 1 "nonmemory_operand" "r,I")))]
  ""
  "@
   cmp\t%0, %1
   cmp\t%0, %1"
)

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_expand "cbranchsi4"
  [(set (reg:CC CONDITION_CODE_REGNUM)
	(compare:CC (match_operand:SI 1 "register_operand"  "")
		    (match_operand:SI 2 "nonmemory_operand" "")))
   (set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
				      [(reg:CC CONDITION_CODE_REGNUM) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "operands[1] = force_reg (Pmode, operands[1]);
   operands[2] = force_reg (Pmode, operands[2]);"
)

(define_insn "*branch_true"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(reg:CC CONDITION_CODE_REGNUM) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "br\t%1, #%b0"
)

(define_insn "*branch_false"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
				      [(reg:CC CONDITION_CODE_REGNUM) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "br\t%1, #%B0"
)

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

;; Subroutine call instruction returning no value.
(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "call_operand" "")
		    (match_operand 1 "" ""))
	      (clobber (reg:SI RETURN_POINTER_REGNUM))])]
  ""
  ""
)

(define_insn "*call_reg"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_POINTER_REGNUM))]
  ""
  "movepc\trret, 8\n\tb\t%0, #al"
)

(define_insn "*call_label"
  [(call (mem:SI (match_operand:SI 0 "call_address_operand" ""))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_POINTER_REGNUM))]
  ""
  "*
{
  if (lih_wl16_operand (operands[0], FUNCTION_MODE))
    return \"lih\trtmp, hi(%0)\n\twl16\trtmp, lo(%0)\n\tmovepc\trret, 8\n\tb\trtmp, #al\";
  else
    return \"movepc\trret, 8\n\tb\t%0, #al\";
}
")

;; Subroutine call instruction returning a value.
(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand"  "=r")
		   (call (match_operand:SI 1 "call_operand" "")
			 (match_operand 2 "" "")))
	      (clobber (reg:SI RETURN_POINTER_REGNUM))])]
  ""
  ""
)

(define_insn "*call_value_reg"
  [(set (match_operand 0 "register_operand"  "=r")
	(call (mem:SI (match_operand:SI 1 "register_operand" "r"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_POINTER_REGNUM))]
  ""
  "movepc\trret, 8\n\tb\t%1, #al"
)

(define_insn "*call_value_label"
  [(set (match_operand 0 "register_operand"  "=r")
	(call (mem:SI (match_operand:SI 1 "call_address_operand" ""))
	      (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_POINTER_REGNUM))]
  ""
  "*
{
  if (lih_wl16_operand (operands[1], FUNCTION_MODE))
    return \"lih\trtmp, hi(%1)\n\twl16\trtmp, lo(%1)\n\tmovepc\trret, 8\n\tb\trtmp, #al\";
  else
    return \"movepc\trret, 8\n\tb\t%1, #al\";
}
")

;; Jump inside a function; an unconditional branch.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "br\t%0, #al"
)

;; Jump to an address through a register
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "address_operand" "r"))]
  ""
  "b\t%0, #al"
)

;; return
(define_insn "return_rret"
  [(parallel [(return) (use (reg:SI RETURN_POINTER_REGNUM))])]
  ""
  "b\trret, #al"
)

(define_insn "return_ib"
  [(return)]
  ""
  "ib"
)

(define_expand "returner"
  [(return)]
  "reload_completed"
  "
{
  enum mist32_function_type fn_type;

  fn_type = mist32_compute_function_type (current_function_decl);
  if (MIST32_INTERRUPT_P (fn_type))
    {
      emit_jump_insn (gen_return_ib ());
      DONE;
    }

  emit_jump_insn (gen_return_rret ());
  DONE;
}
")
