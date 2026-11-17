;;; gptel-macros.el --- Emacs version compatibility macros for gptel         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; Author: Pierre Rouleau;; <prouleau001@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; gptel supports several Emacs versions.  This file provides macros that
;; generate code appropriate for the currently used Emacs version.

;; To use it do:
;;
;; (eval-when-compile (require 'gptel-macros))
;;
;;

;;; Code:

(defmacro gptel--provided-mode-derived-p (mode &rest modes)
  "Non-nil if MODE is derived from a mode that is a member of the list MODES.

A `provided-mode-derived-p' wrapper that returns code compatible with
all Emacs versions:
- On Emacs < 30.1:  (provided-mode-derived-p (mode &rest modes))
- On Emacs ≥ 30.1:  (provided-mode-derived-p (mode modes))"
  (if (< emacs-major-version 30)
      ;;  On Emacs < 30.1:  (provided-mode-derived-p (mode &rest modes))
      (let ((quoted-modes (mapcar (lambda (m)
                                    (if (eq (car-safe m) 'quote)
                                        m
                                      `',m))
                                  modes)))
        `(provided-mode-derived-p ,mode ,@(identity quoted-modes)))
    ;; On Emacs ≥ 30.1:  (provided-mode-derived-p (mode modes))"
    (let ((clean-modes (mapcar (lambda (m)
                                 (if (eq (car-safe m) 'quote)
                                     (cadr m)
                                   m))
                               modes)))
      `(provided-mode-derived-p ,mode ',clean-modes))))

(provide 'gptel-macros)
;;; gptel-macros.el ends here
