;;; julia-formatter.el --- Use JuliaFormatter.jl for julia code  -*- lexical-binding: t; -*-

;; Copyright © 2020  Felipe Lema

;; Author: Felipe Lema <felipe.lema@mortemale.org>
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.0"))
;; URL: https://codeberg.org/FelipeLema/julia-formatter.el
;; Version: 0.1
;; Keywords:

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

;; Leverage JuliaFormatter.jl to indent julia code in Emacs.
;;
;; Provide formatting tools for live coding.  These tools are packed into a service
;; that can be called using JSON-RPC on stdin / stdout.  Exposing JuliaFormatter.jl
;; as a service because the compile time required to get the result of that
;; first format_text() call is considerable and it hinders the coding process.
;;
;; The code that's being formatted must be self-contained (parseable, all if's
;; and while's with a corresponding end).  This is a requirement from
;; JuliaFormatter.jl since it needs to get the AST from the code.
;;
;; The simplest way to use JuliaFormatter.jl is by activating `aggressive-indent-mode'
;; and setting the proper functions
;;
;; Like so:
;; ;; load this file after downloading this package (or installing with straight.el)
;; (require 'julia-formatter)
;; ;; load aggressive indent and setup appropiate variables
;; (julia-formatter-setup-hooks)
;;
;; See https://github.com/domluna/JuliaFormatter.jl

;; This package requires Emacs 27 because previous version of `replace-buffer-contents' is buggy
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237 & https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278


;;

;;; Code:
;;;
(defgroup julia-formatter nil "JuliaFormatter.jl"
  :group 'tools)

(require 'aggressive-indent)
(require 'pcase)
(require 'jsonrpc)

(defvar julia-formatter--server-process-connection
  nil
  "Connection to running server to query for formatter process.

I recommend using JuliaFormatter.jl as a global service because the service has
slow startup and quick response.")

(defun julia-formatter--ensure-server ()
  "Make sure the formatter service is running.

If it's up and running, do nothing."
  (let ((default-directory ;; run at the basename of this script file
          (file-name-as-directory
           (file-name-directory
            ;; https://stackoverflow.com/a/1344894
            (symbol-file 'julia-formatter--ensure-server)))))
    (unless (and julia-formatter--server-process-connection
                 (jsonrpc-running-p julia-formatter--server-process-connection))
      (setq julia-formatter--server-process-connection
            (make-instance
             'jsonrpc-process-connection
             :name "julia formatter server"
             :on-shutdown (lambda (_conn)
                            (message "Julia formatter disconnected"))
             :process (lambda ()
                        (make-process
                         :name "julia formatter server"
                         :command (list "julia"
                                        "--project=."
                                        "formatter_service.jl")
                         :connection-type 'pipe
                         :coding 'utf-8-emacs-unix
                         :noquery t
                         :stderr (get-buffer-create
                                  "*julia formatter server stderr*"))))))))

;;;###autoload
(defun julia-formatter-format-region (begin end)
  "Format region delimited by BEGIN and END  using JuliaFormatter.jl.

Region must have self-contained code.  If not, the region won't be formatted and
will remain as-is."
  (julia-formatter--ensure-server)
  (let* ((text-to-be-formatted
          (buffer-substring-no-properties
           begin end))
         (relative-current-line ;; line number, but relative to BEGIN
          (+ 1
             (-
              (line-number-at-pos)
              (line-number-at-pos begin))))
         (response (jsonrpc-request
                    julia-formatter--server-process-connection
                    :format
                    (list :text
                          (save-match-data
                            (thread-first text-to-be-formatted
                              (split-string  "\n" nil)
                              (vconcat)))
                          :current_line relative-current-line)))
         (as-formatted (mapconcat 'identity response "\n")))
    ;; replace text
    (save-excursion
      (let ((formatting-buffer
             (current-buffer))
            (formatted-region-buffer
             ;; I don't trust `with-temp-buffer', prove me wrong
             (get-buffer-create (format "*formatted julia region %s*"
                                        (string-trim
                                         (shell-command-to-string
                                          "openssl rand -base64 15"))))))
        (with-current-buffer formatted-region-buffer
          (insert as-formatted))
        (with-current-buffer formatting-buffer
          (save-restriction
            (narrow-to-region begin end)
            (replace-buffer-contents
             formatted-region-buffer)))
        (kill-buffer formatted-region-buffer)))))

(defun julia-formatter--defun-range ()
  "Send buffer to service, gen [begin end] of surrounding defun."
  (julia-formatter--ensure-server)
  (let ((response (jsonrpc-request
                   julia-formatter--server-process-connection
                   :defun_range
                   (list :text
                         (save-match-data
                           (thread-first
                               (buffer-substring-no-properties
                                (point-min) (point-max)))
                           (split-string "\n" nil)
                           (vconcat))
                         :position (point)))))
    response))

;;;###autoload
(defun julia-formatter-beginning-of-defun ()
  "Get beginning of surrounding debufn from `julia-formatter--defun-range'."
  (pcase (julia-formatter--defun-range)
    (`[,begin ,_]
     (goto-char
      begin))))

;;;###autoload
(defun julia-formatter-end-of-defun ()
  "Get beginning of surrounding debufn from `julia-formatter--defun-range'."
  (pcase (julia-formatter--defun-range)
    (`[,_ ,end]
     (goto-char end))))

;;;###autoload
(defun julia-formatter-setup-aggressive-indent-in-buffer ()
  "Activate `aggressive-indent-mode' in this buffer and setup defun functions.

Setup `beginning-of-defun-function', `end-of-defun-function' &
`indent-region-function' to use the formatter service."
  (julia-formatter--ensure-server)
  (setq-local beginning-of-defun-function #'julia-formatter-beginning-of-defun)
  (setq-local end-of-defun-function #'julia-formatter-end-of-defun)
  (add-hook 'aggressive-indent-modes-to-prefer-defun
            'julia-mode)
  (setq-local indent-region-function #'julia-formatter-format-region)
  (unless aggressive-indent-mode
    (aggressive-indent-mode)))

;;;###autoload
(defun julia-formatter-setup-hooks ()
  "Setup hooks for using JuliaFormater.jl in julio-mode."
  ;; Start server in the background as soon as possible
  (add-hook 'after-init-hook
            #'julia-formatter--ensure-server)
  ;; setup agressive-indent + formatter for julia-mode
  (add-hook 'julia-mode-hook
            #'julia-formatter-setup-aggressive-indent-in-buffer))

(provide 'julia-formatter)
;;; julia-formatter.el ends here
