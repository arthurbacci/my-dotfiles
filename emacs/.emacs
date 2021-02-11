


(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
(add-to-list 'load-path "~/.emacs.d/")
(require 'langtool)

                                        ;Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

    ; For loading ~/.emacs.d/themes themes
    ; https://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

					; For loading themes in emacs --daemon


    ; For removing welcome screen
(setq inhibit-startup-message t)

    ; For removing menus
(tool-bar-mode -1)
(menu-bar-mode -1)

    ; For removing scroll bar
(scroll-bar-mode -1)

    ; Line numbers
(global-linum-mode t)

    ; Don't creates files with ~ in the end, backups in ~/.emacs.d/backup instead
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

    ; Show columns
(setq column-number-mode t)

    ; stop creating those #auto-save# files
(setq auto-save-default nil)

    ; stop creating .#lock files
(setq create-lockfiles nil)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;(set-frame-parameter (selected-frame) 'alpha '80)
;(add-to-list 'default-frame-alist '(alpha . 80))

(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '80 '100))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

(global-wakatime-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" default))
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(gitignore-mode php-mode fixmee wakatime-mode ewal markdown-preview-mode markdown-mode+ markdown-mode arduino-mode pdf-tools haskell-mode haskell-emacs))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 98 :width normal)))))

;(setq ewal-use-built-in-on-failure-p t)
;(ewal-load-color 'magenta +4)
