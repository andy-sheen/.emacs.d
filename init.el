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

;;
;; Setup packages
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(setq use-package-always-ensure t)

(setq custom-safe-themes t)

;;
;; Locale and Encoding
;;
(set-locale-environment "en_GB.UTF-8")
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
;;
;; Define a function to call every time a  frame is created
(defun my-load-on-frame (frame)
  (load-theme 'wombat t)
  )

(add-to-list 'after-make-frame-functions 'my-load-on-frame)

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
                ("\\.js$" . web-mode)
                ("\\.htm$" . web-mode)    ; Used to be html-mode
                ("\\.html$" . web-mode)   ; Used to be html-mode
                ("\\.css$" . css-mode)
                ("^[Mm]akefile" . makefile-mode)
                ("\\.ahk$" . xahk-mode)
                ("\\.ad$" . adoc-mode)
                ("\\.groovy$" . groovy-mode)
                ) auto-mode-alist ))

(add-to-list 'magic-mode-alist '( "\[[A-Za-z0-9]\+\]" . conf-unix-mode))

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
      ;;
      ;;   Set up printers to point to the correct printer
      (if (string= (upcase system-name) "BUSTER")
          (setq printer-name "//buster/m476dw (HP Color LaserJet MFP M476dw)"))
      )
  ;; Could put else in here
)

;;
;; Setup Cygwin environment
;;
(if (eq system-type 'cygwin)
    (progn
      ;;
      ;; This removes unsightly ^M characters that would otherwise
      ;; appear in the output of java applications.
      ;;
      (add-hook 'comint-output-filter-functions
                'comint-strip-ctrl-m)
      ;;
      ;; Set an appropriate temporary file
      (setq temporary-file-directory "/cygdrive/c/Temp/")
      (setq magit-git-executable "/cygdrive/c/cygwin64/bin/git.exe")

      (if (string= (upcase system-name) "BUSTER")
          (setq printer-name "//buster/m476dw (HP Color LaserJet MFP M476dw)"))
      )
  ;; Could put else in here
)

;; .---------------------------------------------------------------------------
;; |   adoc-mode
;; `---------------------------------------------------------------------------
;
(autoload 'adoc-mode "adoc-mode" nil t)

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

;; .---------------------------------------------------------------------------
;; | compilation
;; `---------------------------------------------------------------------------
(setq compilation-auto-jump t)

;; .---------------------------------------------------------------------------
;; | completion
;; `---------------------------------------------------------------------------
;; (require 'auto-complete-config)
;; ;;
;; ;; Turn off fuzzy matching
;; (setq ac-use-fuzzy nil)
;; ;; match case always
;; (setq ac-ignore-case nil)
;; ;; Don't show menu
;; (setq ac-auto-show-menu nil)
;; ;; Don't start unless explicitly called
;; (setq ac-auto-start 5)
;; (global-set-key "\M-1" 'auto-complete)
;; ;;
;; ;; Use a specific menu map when the menu is displayed
;; (setq ac-use-menu-map t)
;; ;; Default settings
;; (define-key ac-menu-map "\C-n" 'ac-next)
;; (define-key ac-menu-map "\C-p" 'ac-previous)
;; (define-key ac-menu-map [down] 'ac-next)
;; (define-key ac-menu-map [up] 'ac-previous)
;; ;;
;; ;; And remove [up] and [down] from the completing map all together
;; (define-key ac-completing-map [down] nil)
;; (define-key ac-completing-map [up] nil)


;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac-dict")
;; (ac-config-default)

;; .---------------------------------------------------------------------------
;; | company
;; `---------------------------------------------------------------------------
;;(add-hook 'after-init-hook 'global-company-mode)
;(add-hook 'c-mode-hook 'company-mode)
;(add-hook 'c++-mode-hook 'company-mode)
;(setq company-idle-delay 0.1)
;;(setq company-backends (delete 'company-semantic company-backends))
;(define-key c-mode-map  [(tab)] 'company-complete)
;(define-key c++-mode-map  [(tab)] 'company-complete)

;(eval-after-load 'company
;  '(progn
;     (define-key company-mode-map (kbd "M-'") 'helm-company)
;     (define-key company-active-map (kbd "M-'") 'helm-company)))

;; ;; .---------------------------------------------------------------------------
;; ;; |   cscope
;; ;; `---------------------------------------------------------------------------

;; ;;
;; ;; Override the code to say we'll manage it all
;; (setq cscope-option-do-not-update-database t)
;; (setq cscope-option-use-inverted-index t)
;; (require 'xcscope)

;; (cscope-setup)

;; (defun setup-cscope-keys (map)
;;   "Sets up the keys according to map"
;;   ;;
;;   ;;    The following will add "f#" and "C-f#" keybindings, which are
;;   ;;    easier to type than the usual "C-c s" prefixed keybindings.
;;   ;;
;;   ;; Segment 2 - finding functions
;;   ;;
;;   (define-key map [f5] 'cscope-find-global-definition-no-prompting)
;;   (define-key map [(ctrl f5)] 'cscope-find-global-definition)
;;   (define-key map [f6] 'cscope-find-this-symbol)
;;   (define-key map [(ctrl f6)] 'cscope-find-assignments-to-this-symbol)
;;   (define-key map [f7] 'cscope-find-functions-calling-this-function)
;;   (define-key map [(ctrl f7)] 'cscope-find-called-functions)
;;   (define-key map [f8] ' cscope-find-this-text-string)
;;   (define-key map [(ctrl f8)] 'cscope-find-files-including-file)
;;   ;;
;;   ;; Segment 3 - moving around functions
;;   ;;
;;   (define-key map [f9] 'cscope-history-backward-line-current-result)
;;   (define-key map [(ctrl f9)] 'cscope-history-backward-file-current-result)
;;   (define-key map [f10] 'cscope-history-forward-line-current-result)
;;   (define-key map [(ctrl f10)] 'cscope-history-forward-file-current-result)
;;   (define-key map [f11] 'cscope-set-initial-directory)
;;   (define-key map [(ctrl f11)] 'cscope-unset-initial-directory)
;;   (define-key map [f12] 'cscope-pop-mark)
;;   (define-key map [(ctrl f12)] 'cscope-display-buffer)
;;   )

;; ;; And add the above to both the minor mode and the cscope buffer
;; (setup-cscope-keys cscope-minor-mode-keymap)
;; (setup-cscope-keys cscope-list-entry-keymap)

;; ;;
;; ;; Left to assign:
;; ;;   C-c s D         cscope-dired-directory
;; ;;   C-c s E         cscope-edit-list-of-files-to-index
;; ;;   C-c s I         cscope-index-files
;; ;;   C-c s L         cscope-create-list-of-files-to-index
;; ;;   C-c s S .. C-c s T              cscope-tell-user-about-directory
;; ;;   C-c s W         cscope-tell-user-about-directory
;; ;;   C-c s a         cscope-set-initial-directory
;; ;;   C-c s b         cscope-display-buffer
;; ;;   C-c s e         cscope-find-egrep-pattern
;; ;;   C-c s f         cscope-find-this-file


;; .---------------------------------------------------------------------------
;; |   deft
;; `---------------------------------------------------------------------------
(use-package deft
  :config (setq deft-auto-save-interval 300.0))


;; .---------------------------------------------------------------------------
;; |   doxymacs
;; `---------------------------------------------------------------------------
;; (require 'doxymacs)
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;; (add-hook 'c-mode-common-hook 'doxymacs-mode)

;; .---------------------------------------------------------------------------
;; |   flyspell
;; `---------------------------------------------------------------------------
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'adoc-mode-hook (lambda () (flyspell-mode 1)))
(use-package flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "M-'") 'flyspell-auto-correct-previous-word)

;; .---------------------------------------------------------------------------
;; |   helm see: http://tuhdo.github.io/helm-intro.html
;; `---------------------------------------------------------------------------
(use-package helm
  :bind
  (("C-c h"     . helm-command-prefix)
   ("M-x"       . helm-M-x)
   ("C-x b"     . helm-mini)
   ("C-x C-b"   . helm-buffers-list)
   ("C-x C-f"   . helm-find-files)
   ("C-c <SPC>" . helm-all-mark-rings))
  :config
  ;; C-x c is dangerously close to C-x C-c.
  (global-unset-key (kbd "C-x c"))

  ;; Helm navigation/actions.
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  ;; C-i is TAB in a terminal.
  (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t)

  (helm-mode 1))

;; helm-swoop is no longer in MELPA, so bootstrap it from Emacs Attic.
(unless (package-installed-p 'helm-swoop)
  (package-vc-install
   "https://github.com/emacsattic/helm-swoop.git"))

(use-package helm-swoop
  :ensure nil

  :bind
  (("C-f"     . helm-swoop)
   ("M-I"     . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("M-i"     . helm-multi-swoop-all))

  :config
  ;; Search integration
  (define-key isearch-mode-map (kbd "M-i")
              #'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i")
              #'helm-multi-swoop-all-from-helm-swoop)

  ;; Navigate results like isearch
  (define-key helm-swoop-map (kbd "C-p") #'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-n") #'helm-next-line)
  (define-key helm-swoop-map (kbd "C-f") #'helm-next-line)

  (define-key helm-multi-swoop-map (kbd "C-p") #'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-n") #'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-f") #'helm-next-line)

  :custom
  (helm-multi-swoop-edit-save t)
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-split-direction 'split-window-vertically)
  (helm-swoop-speed-or-color nil)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-line-number-face t)
  (helm-swoop-pre-input-function (lambda () "")))

;; .---------------------------------------------------------------------------
;; |   magit mode
;; `---------------------------------------------------------------------------
(use-package magit)

;; .---------------------------------------------------------------------------
;; |   paren mode
;; `---------------------------------------------------------------------------
(show-paren-mode 1)
(setq show-paren-delay 0.5)
(setq show-paren-context-when-offscreen 'echo-area)

;; .---------------------------------------------------------------------------
;; |   sr-speedbar
;; `---------------------------------------------------------------------------
(setq sr-speedbar-skip-other-window-p t)

;; .---------------------------------------------------------------------------
;; |   time setup
;; `---------------------------------------------------------------------------
(display-time)
(setq-default display-time-day-and-date t)
;;
;; Note: this function used to be bound to F2 (see below)
(setq display-time-world-list '(
                                ("America/Los_Angeles" "Irvine/SD")
                                ("US/Eastern" "Andover")
                                ("Europe/London" "London")
                                ("Asia/Jerusalem" "Israel")
                                )
)

(setq-default inhibit-startup-message t)


;; .---------------------------------------------------------------------------
;; |   tramp
;; `---------------------------------------------------------------------------
;;
(setq tramp-default-method "ssh")

;; .---------------------------------------------------------------------------
;; |   web-mode
;; `---------------------------------------------------------------------------
;;
(setq web-mode-enable-engine-detection t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")
        ("blade"  . "\\.blade\\."))
      )
;; .---------------------------------------------------------------------------
;; |   whitespace
;; `---------------------------------------------------------------------------
;;
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

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

;;(define-key global-map "\C-f" 'isearch-forward)
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

;; (defun key-f2 ()
;;   (interactive)
;;   (display-time-world))

(defun key-f3 ()
  (interactive)
  (recompile))

(defun key-c-f3 ()
  (interactive)
  (compile))

(defun key-f4 ()
  (interactive)
  (sr-speedbar-toggle))

;; (defun key-f5 ()
;;   (interactive)
;;   (helm-resume))

;; (defun key-f6 ()
;;   (interactive)
;;   (magit-status))

(defun key-f7 ()
  (interactive)
  (load-theme 'wombat t))

(defun key-f8 ()
  (interactive)
  (scroll-right))

(defun key-f9 ()
  (interactive)
  (scroll-left))

(defun key-f10 ()
  (interactive)
  (deft))

(defun key-f11 ()
  (interactive)
  (whitespace-mode 'toggle))

(defun key-f12 ()
  (interactive)
  (delete-frame))

;; And set the colours....
;(load-theme 'wombat t)

(define-key global-map [f1] 'key-f1)
(define-key global-map [f2] 'key-f2)
(define-key global-map [f3] 'key-f3)

(define-key global-map [f4] 'key-f4)
(define-key global-map [f5] 'helm-resume)
(define-key global-map [f6] 'magit-status)
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

;; Terminal Emacs resets faces during startup.
;; After 10+ years of pressing F7, just load Wombat after it settles.
(run-with-idle-timer
 1 nil
 (lambda ()
   (load-theme 'wombat t)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(deft flyspell-correct-helm helm-swoop magit magit-gerrit php-mode))
 '(package-vc-selected-packages
   '((helm-swoop :vc-backend Git :url
                 "https://github.com/emacsattic/helm-swoop.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
