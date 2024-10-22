;; -*- Scheme -*-
;; Predicate definitions for mist32.
;; Copyright (C) 2012 Free Software Foundation, Inc.
;; Contributed by Hirotaka Kawata <hirotaka@techno-st.net>
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.


;; Acceptable arguments to the call insn.
(define_predicate "call_operand"
  (match_code "reg,symbol_ref,label_ref,const_int")
)

(define_predicate "call_address_operand"
  (match_code "symbol_ref,label_ref,const_int")
)

;; General register operand
(define_predicate "general_register_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return GPR_P(REGNO(op));
})

;; Stack pointer register operand
(define_predicate "stack_pointer_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == STACK_POINTER_REGNUM")))

(define_predicate "register_operand_not_sp"
  (and (match_code "reg")
       (match_test "REGNO (op) != STACK_POINTER_REGNUM")))

;; Displacement immediate
(define_predicate "disp8_operand"
  (match_code "const_int")
{
  if (CONST_INT_P (op)
      && !(XINT (op, 0) & 0x3)
      && XINT (op, 0) <= 127
      && XINT (op, 0) >= -128)
    return 1;

   return 0;
})
(define_predicate "disp7_operand"
  (match_code "const_int")
{
  if (CONST_INT_P (op)
      && !(XINT (op, 0) & 0x1)
      && XINT (op, 0) <= 63
      && XINT (op, 0) >= -64)
    return 1;

   return 0;
})
(define_predicate "disp6_operand"
  (match_code "const_int")
{
  if (CONST_INT_P (op)
      && XINT (op, 0) <= 31
      && XINT (op, 0) >= -32)
    return 1;

   return 0;
})

;; Returns true if OP is a symbol reference.
(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST :
      return 1;

    default:
      return 0;
    }
})

;; Returns true if OP is an acceptable operand for two insn (lih + w16l).
(define_predicate "lih_wl16_operand"
  (match_code "symbol_ref,label_ref,const_int")
{
  if (GET_CODE (op) == SYMBOL_REF
      || GET_CODE (op) == LABEL_REF)
    return 1;

  if (CONST_INT_P (op)
      && (satisfies_constraint_J (op)
          || satisfies_constraint_K (op)
          || satisfies_constraint_L (op)))
    return 0;

  return 1;
})

;; mist32 O2/I11 operand
(define_predicate "o2_i11_operand"
  (match_code "reg,const_int")
{
  if (GET_CODE (op) == REG || satisfies_constraint_I (op))
    return 1;
  return 0;
})

(define_predicate "o2_ui11_operand"
  (match_code "reg,const_int")
{
  if (GET_CODE (op) == REG || satisfies_constraint_M (op))
    return 1;
  return 0;
})

;; For address offset
(define_predicate "small_offset_operand"
  (match_code "const_int")
{
  if (satisfies_constraint_I (op))
    return 1;
  return 0;
})
