;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .---------------------------------------------------------------------------
;; |   setup the environment
;; `---------------------------------------------------------------------------
;
; emacs specific display tweaks
;
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;(require 'custom)
;;;;
;(setq debug-on-error t)
;;
;; Size of the initial frame
(setq default-frame-alist
      '(
	;;(top . 100) (left . 100)
	(width . 140) (height . 60)
	(cursor-color . "black")
	(cursor-type . box)
	(foreground-color . "black")
	;;                  (background-color . "black")
	))

;
; Set up printers to point to the correct printer
;(setq printer-name "//buster/ColourLaser")

;;
;; Turn on column numbering
(setq column-number-mode t)


;; .---------------------------------------------------------------------------
;; |   auto mode loading
;; `---------------------------------------------------------------------------
;
(setq auto-mode-alist
      (append '(("\\.txt$" . text-mode)
		("\\.cc$\\|\\.C$" . c++-mode)
		("\\.c$\\|\\.h$" . c-mode)
		("\\.js$" . c-mode)
		("\\.htm$" . html-helper-mode)
		("\\.html$" . html-helper-mode)
		("\\.css$" . css-mode)
		("^[Mm]akefile" . makefile-mode)
		("\\.ahk$" . xahk-mode)
		) auto-mode-alist ))

(add-to-list 'magic-mode-alist '( "\[[A-Za-z0-9]\+\]" . conf-unix-mode))
;;
;; HTML mode
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

;;
;; Add auto fill on text mode
(setq text-mode-hook '(lambda () (auto-fill-mode 1)))

;; .---------------------------------------------------------------------------
;; |   Setup cygwin properly for Emacs
;; `---------------------------------------------------------------------------
;; see http://cygwin.com/faq/faq-nochunks.html#faq.using.ntemacs


(if (eq system-type 'windows-nt)
    (progn
      ;; WARNING:The latest version of bash sets and uses the environment variable PID.
      ;; For some as yet unknown reason, if PID is set and Emacs passes it on to bash subshells,
      ;; bash croaks (Emacs can inherit the PID variable if it's started from a bash shell).
      ;; If you clear the PID variable in your startup file,
      ;; you should be able to continue to use bash as your subshell:
      (setenv "PID" nil)

      ;; This assumes that Cygwin is installed in C:\cygwin (the
      ;; default) and that C:\cygwin\bin is not already in your
      ;; Windows Path (it generally should not be).
      ;;
      (setq exec-path (cons "C:/cygwin/bin" (cons "C:/cygwin64/bin" exec-path)))
      (setenv "PATH" (concat "C:\\cygwin64\\bin;" "C:\\cygwin\\bin;" (getenv "PATH")))
      (setenv "TEMP" "/cygdrive/c/Temp/")
      ;;
      ;; NT-emacs assumes a Windows command shell, which you change
      ;; here.
      ;;
      (setq process-coding-system-alist '(("bash" . undecided-unix)))
      (setq shell-file-name "bash")
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)

      ;;
      ;; This removes unsightly ^M characters that would otherwise
      ;; appear in the output of java applications.
      ;;
      (add-hook 'comint-output-filter-functions
                'comint-strip-ctrl-m)
      ;;
      ;; Set an appropriate temporary file
      (setq temporary-file-directory "/cygdrive/c/Temp/")

      ;;
      ;; Add in support for cygwin paths
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      )
  ;; Could put else in here
)
;; .---------------------------------------------------------------------------
;; |   Ahk-mode
;; `---------------------------------------------------------------------------
;
(autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotKey scripts." t)
(defalias 'ahk-mode 'xahk-mode)

;; .---------------------------------------------------------------------------
;; |   Backup settings
;; `---------------------------------------------------------------------------
;
(setq vc-make-backup-files t)

(setq
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 3
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq backup-directory-alist '(("" . "~/restricted/.emacs/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/restricted/.emacs/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer))
)

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; .---------------------------------------------------------------------------
;; |   cc-mode
;; `---------------------------------------------------------------------------
;;
;; set default coding style
(setq c-default-style "k&r"
      c-basic-offset 4)

;;
;; Strip trailing whitespace on save on all cc-mode
(add-hook 'c-mode-common-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
;;
;; and remove any indent when in an external language in C
(add-hook 'c-mode-hook (lambda () (c-set-offset 'inextern-lang 0)))

(require 'cc-mode)
;; (autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
;; (autoload 'c-mode   "cc-mode" "C Editing Mode" t)

;; .---------------------------------------------------------------------------
;; | compilation
;; `---------------------------------------------------------------------------
(setq compilation-auto-jump t)

;; .---------------------------------------------------------------------------
;; | completion
;; `---------------------------------------------------------------------------
(require 'auto-complete-config)
;;
;; Turn off fuzzy matching
(setq ac-use-fuzzy nil)
;; match case always
(setq ac-ignore-case nil)
;; Don't show menu
(setq ac-auto-show-menu nil)
;; Don't start unless explicitly called
(setq ac-auto-start 5)
(global-set-key "\M-1" 'auto-complete)
;;
;; Use a specific menu map when the menu is displayed
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\M-n" 'ac-next)
(define-key ac-menu-map "\M-p" 'ac-previous)
(define-key ac-menu-map [down] 'ac-next)
(define-key ac-menu-map [up] 'ac-previous)
;;
;; And remove [up] and [down] from the completing map all together
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)


(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac-dict")
(ac-config-default)

;; .---------------------------------------------------------------------------
;; |   cscope
;; `---------------------------------------------------------------------------

;;
;; Override the code to say we'll manage it all
(setq cscope-option-do-not-update-database t)
(setq cscope-option-use-inverted-index t)
(require 'xcscope)

(cscope-setup)

(defun setup-cscope-keys (map)
  "Sets up the keys according to map"
  ;;
  ;;    The following will add "f#" and "C-f#" keybindings, which are
  ;;    easier to type than the usual "C-c s" prefixed keybindings.
  ;;
  ;; Segment 2 - finding functions
  ;;
  (define-key map [f5] 'cscope-find-global-definition-no-prompting)
  (define-key map [(ctrl f5)] 'cscope-find-global-definition)
  (define-key map [f6] 'cscope-find-this-symbol)
  (define-key map [(ctrl f6)] 'cscope-find-assignments-to-this-symbol)
  (define-key map [f7] 'cscope-find-functions-calling-this-function)
  (define-key map [(ctrl f7)] 'cscope-find-called-functions)
  (define-key map [f8] ' cscope-find-this-text-string)
  (define-key map [(ctrl f8)] 'cscope-find-files-including-file)
  ;;
  ;; Segment 3 - moving around functions
  ;;
  (define-key map [f9] 'cscope-history-backward-line-current-result)
  (define-key map [(ctrl f9)] 'cscope-history-backward-file-current-result)
  (define-key map [f10] 'cscope-history-forward-line-current-result)
  (define-key map [(ctrl f10)] 'cscope-history-forward-file-current-result)
  (define-key map [f11] 'cscope-set-initial-directory)
  (define-key map [(ctrl f11)] 'cscope-unset-initial-directory)
  (define-key map [f12] 'cscope-pop-mark)
  (define-key map [(ctrl f12)] 'cscope-display-buffer)
  )

;; And add the above to both the minor mode and the cscope buffer
(setup-cscope-keys cscope-minor-mode-keymap)
(setup-cscope-keys cscope-list-entry-keymap)

;;
;; Left to assign:
;;   C-c s D         cscope-dired-directory
;;   C-c s E         cscope-edit-list-of-files-to-index
;;   C-c s I         cscope-index-files
;;   C-c s L         cscope-create-list-of-files-to-index
;;   C-c s S .. C-c s T              cscope-tell-user-about-directory
;;   C-c s W         cscope-tell-user-about-directory
;;   C-c s a         cscope-set-initial-directory
;;   C-c s b         cscope-display-buffer
;;   C-c s e         cscope-find-egrep-pattern
;;   C-c s f         cscope-find-this-file


;; .---------------------------------------------------------------------------
;; |   doxymacs
;; `---------------------------------------------------------------------------
(require 'doxymacs)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

;; .---------------------------------------------------------------------------
;; |   help+
;; `---------------------------------------------------------------------------
(require 'help+)
(require 'help-fns+)
(require 'help-macro+)
(require 'help-mode+)

;; .---------------------------------------------------------------------------
;; |   paren mode
;; `---------------------------------------------------------------------------
(show-paren-mode 1)
(setq show-paren-delay 0.5)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
        "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
        (interactive)
        (let* ((cb (char-before (point)))
               (matching-text (and cb
                                   (char-equal (char-syntax cb) ?\) )
                                   (blink-matching-open))))
                  (when matching-text (message matching-text))))
;; .---------------------------------------------------------------------------
;; |   packages
;; `---------------------------------------------------------------------------
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; .---------------------------------------------------------------------------
;; |   php mode
;; `---------------------------------------------------------------------------
(require 'php-mode)

;; If you want colorization, turn on global-font-lock or
;; add this to your .emacs:
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; To use abbrev-mode, add lines like this:
;;   (add-hook 'php-mode-user-hook
;;     '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;; .---------------------------------------------------------------------------
;; |   time setup
;; `---------------------------------------------------------------------------
(display-time)
(setq-default display-time-day-and-date t)
;;
;; Note: this function is bound to F2 (see below)
(setq display-time-world-list '(
				("America/Los_Angeles" "Irvine/SD")
				("US/Eastern" "Andover")
				("Europe/London" "London")
				("Asia/Jerusalem" "Israel")
				)
)

(setq-default inhibit-startup-message t)


;; .---------------------------------------------------------------------------
;; |   Funky keyboard stuff
;; `---------------------------------------------------------------------------
;;
;; Move between multiple windows easily
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; .---------------------------------------------------------------------------
;; |   key bindings
;; `---------------------------------------------------------------------------
(define-key global-map "\eOA" 'previous-line)
(define-key global-map "\eOB" 'next-line)
(define-key global-map "\eOC" 'forward-char)
(define-key global-map "\eOD" 'backward-char)

(define-key global-map "\C-xs" 'save-buffer)
(define-key global-map "\M-l" 'goto-line)

(define-key global-map "\C-f" 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)

(define-key global-map "\M-#" 'menu-bar-open)

(global-set-key (kbd "<select>") 'move-end-of-line)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;;
;; Keypad
(define-key global-map [kp-8] 'previous-line)
(define-key global-map [kp-2] 'next-line)
(define-key global-map [kp-6] 'forward-char)
(define-key global-map [kp-4] 'backward-char)
(define-key global-map [kp-7] 'beginning-of-buffer)
(define-key global-map [kp-1] 'end-of-buffer)
(define-key global-map [kp-9] 'scroll-down)
(define-key global-map [kp-3] 'scroll-up)
(define-key global-map "\e[1~" 'scroll-down)
(define-key global-map "\e[2~" 'beginning-of-buffer)
(define-key global-map "\e[3~" 'scroll-down)
(define-key global-map "\e[4~" 'backward-delete-char)
(define-key global-map "\e[5~" 'end-of-buffer)
(define-key global-map "\e[6~" 'scroll-up)

;;
;; Function keys

(defun key-f1 ()
  (interactive)
  (info))

(defun key-f2 ()
  (interactive)
  (display-time-world))

(defun key-f3 ()
  (interactive)
  (recompile))

(defun key-c-f3 ()
  (interactive)
  (compile))

(defun key-f4 ()
  (interactive)
  (message "f4"))

(defun key-f5 ()
  (interactive)
  (message "f5"))

(defun key-f6 ()
  (interactive)
  (message "f6"))

(defun key-f7 ()
  (interactive)
  (load-theme 'whiteboard t))

(defun key-f8 ()
  (interactive)
  (scroll-right))

(defun key-f9 ()
  (interactive)
  (scroll-left))

(defun key-f10 ()
  (interactive)
  (message "f10"))

(defun key-f11 ()
  (interactive)
  (message "f11"))

(defun key-f12 ()
  (interactive)
  (message "f12"))

(define-key global-map [f1] 'key-f1)
(define-key global-map [f2] 'key-f2)
(define-key global-map [f3] 'key-f3)
(define-key global-map [f4] 'key-f4)
(define-key global-map [f5] 'key-f5)
(define-key global-map [f6] 'key-f6)
(define-key global-map [f7] 'key-f7)
(define-key global-map [f8] 'key-f8)
(define-key global-map [f9] 'key-f9)
(define-key global-map [f10] 'key-f10)
(define-key global-map [f11] 'key-f11)
(define-key global-map [f12] 'key-f12)

(define-key global-map "\e[224z" 'key-f1)
(define-key global-map "\e[225z" 'key-f2)
(define-key global-map "\e[226z" 'key-f3)
(define-key global-map "\e[227z" 'key-f4)
(define-key global-map "\e[228z" 'key-f5)
(define-key global-map "\e[229z" 'key-f6)
(define-key global-map "\e[230z" 'key-f7)
(define-key global-map "\e[231z" 'key-f8)
(define-key global-map "\e[232z" 'key-f9)
(define-key global-map "\e[233z" 'key-f10)
(define-key global-map "\e[234z" 'key-f11)
(define-key global-map "\e[235z" 'key-f12)

;;
;; These apear to be C-F1.. C-F4
(define-key global-map "\e[1;5p" 'key-f1)
(define-key global-map "\e[1;5q" 'key-f2)
(define-key global-map "\e[1;5r" 'key-c-f3) ; Recompile
(define-key global-map "\e[1;5s" 'key-f4)

;; .---------------------------------------------------------------------------
;; |   Emacs server
;; `---------------------------------------------------------------------------

;; Put all this here as gnuserv-start may fail. If it does,
;; the rest of the file will fail to load.
;(autoload 'server-start "gnuserv" "" t)

;; (setq server-host "buster.home")
;; ;set server-use-tcp to t
;; (setq server-use-tcp t)
;; (setq server-auth-dir "//mainserver.home/temp/serverauth/")
;(require 'gnuserv)
;(gnuserv-start)

;;
;; More modern server start
;(require 'server)
;(server-start)

;; .---------------------------------------------------------------------------
;; |   the end
;; `---------------------------------------------------------------------------



(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)
