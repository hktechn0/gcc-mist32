;; Constraint definitions for mist32.
;; Copyright (C) 2012 Free Software Foundation, Inc.
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

;; Register constraints
(define_register_constraint "k" "SP_REG"
  "Stack pointer register")

;; Integer constraints
(define_constraint "I"
  "11-bit signed immediate."
  (and (match_code "const_int")
       (match_test "ival >= -0x400 && ival <= 0x3ff")))

(define_constraint "J"
  "16-bit signed immediate."
  (and (match_code "const_int")
       (match_test "ival >= -0x8000 && ival <= 0x7fff")))

(define_constraint "K"
  "16-bit unsigned immediate."
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 0xffff")))

(define_constraint "L"
  "16-bit signed immediate left shifted by 16."
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0")
       (match_test "(ival >> 16) >= -0x8000 && (ival >> 16) <= 0x7fff")))

(define_constraint "M"
  "11-bit unsigned immediate. (for logic?)"
  (and (match_code "const_int")
       (match_test "(unsigned HOST_WIDE_INT) ival <= 0x7ff")))

(define_constraint "O"
  "constant zero"
  (and (match_code "const_int")
       (match_test "ival == 0")))

;; Extra constraints.
(define_constraint "Q"
  "symbol_ref"
  (and (match_code "mem")
       (match_code "symbol_ref" "0")))

(define_constraint "R"
  "offsettable symbolic address constant"
  (and (match_code "const")
       (match_test "GET_CODE (XEXP(op, 0)) == PLUS")
       (match_test "symbolic_operand (XEXP (XEXP (op, 0), 0), Pmode)")
       (match_test "CONST_INT_P (XEXP (XEXP (op, 0), 1))"))
)

(define_constraint "T"
  "An indirect of a pointer."
  (and (match_code "mem")
       (match_code "reg" "0"))
)
