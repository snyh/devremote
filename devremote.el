;;; devremote.el --- Write on home and test on remote server in Emacs.
;;; Commentary:
;;    automatically transfer to remote server.
;;

(require 'subr-x)
(require 'dash)

(cl-defstruct -pinfo
  server
  local-root-dir
  ignore-dirs
  remote-root-dir)

(defun -expand-pinfo (i)
  (let ((local-root (expand-file-name (-pinfo-local-root-dir i)))
        (ignore-dirs (-pinfo-ignore-dirs i))
        (server (-pinfo-server i)))
    (make--pinfo
     :server server
     :local-root-dir local-root
     :ignore-dirs (mapcar (lambda (elt) (expand-file-name (concat local-root "/" elt)))
                          ignore-dirs)
     :remote-root-dir (concat "/ssh:" server ":" (-pinfo-remote-root-dir i))
     )))

(setq devremote-project-infos
      (mapcar '-expand-pinfo
              (list (make--pinfo
                 :server "sw-sh"
                 :local-root-dir "~/codes/node"
                 :ignore-dirs '(".git/*" ".out/*")
                 :remote-root-dir "~/snyh/node")

                (make--pinfo
                 :server "sw-sh"
                 :local-root-dir "~/codes/go-sw64"
                 :ignore-dirs '(".git/*" "pkg/*")
                 :remote-root-dir "~/snyh/go-sw64")
                )))


(defun -in-dir(f dir) (string-prefix-p dir f))
(defun -not-in-dir (f dir) (not (-in-dir f dir)))

(defun --in-project-p (_file _local-root _ignore-dirs)
  (seq-reduce (lambda (acc elt)
                (and acc (-not-in-dir _file elt)))
              _ignore-dirs
              (-in-dir _file _local-root)
              ))
(defun --pinfo-contain-file (pinfo file)
  (let ((_local-root (-pinfo-local-root-dir pinfo))
        (_ignore-dirs (-pinfo-ignore-dirs pinfo))
        (_file (expand-file-name file))
        )
    (--in-project-p _file _local-root _ignore-dirs)))


(defun devremote-query-pinfo (file)
  (-first (lambda (pinfo) (--pinfo-contain-file pinfo file))
          devremote-project-infos))

(defun in-any-project-p (file) (devremote-query-pinfo file))

(defun pinfo-base-name (name pinfo)
  (file-relative-name name (-pinfo-local-root-dir pinfo)))

(defun pinfo-local-file (name pinfo)
  (expand-file-name
   (concat
    (-pinfo-local-root-dir pinfo)
    "/"
    (pinfo-base-name name pinfo))))

(defun pinfo-remote-file (name pinfo)
  (expand-file-name
   (concat
    (-pinfo-remote-root-dir pinfo)
    "/"
    (pinfo-base-name name pinfo))))

(defun -copy-to-remote (long-name)
  (let ((pinfo (devremote-query-pinfo long-name)))
    (if pinfo
        (copy-file
         (pinfo-local-file long-name pinfo)
         (pinfo-remote-file long-name pinfo)
         t)
      (user-error "%s is not in any project" long-name)
      )))

(defun devremote-sync-all ()
  (interactive)
  (let* (
         (buffer-name "*DEVREMOTE-SYNC*")
         (long-name buffer-file-name)
         (pinfo (devremote-query-pinfo long-name))
         )
    (unless pinfo (user-error (format "%s isn't in any project" long-name)))
    (let* ((local-root-dir (-pinfo-local-root-dir pinfo))
          (remote-root-dir (-pinfo-remote-root-dir pinfo))
          (ignore-dirs (-pinfo-ignore-dirs pinfo))
          (cmd (-build-rsync-commandline local-root-dir remote-root-dir ignore-dirs)))
      (switch-to-buffer-other-window buffer-name)
      (erase-buffer)
      (insert (format "BEGIN %s at\n %s\n"
                      cmd
                      (shell-command-to-string "echo -n $(date)")))
      (start-process-shell-command "DEVREMOTE-SYNC"
                                   buffer-name
                                   cmd))))

(defun -build-rsync-commandline (local-dir remote-dir ignore-dirs)
  (let ((ignores (-reduce-from
                  (lambda (acc elt)
                    (format "%s --exclude=\"%s\""
                            acc
                            (string-remove-prefix (concat local-dir "/") elt)))
                  ""
                  ignore-dirs)))
    (format "rsync -Pazv %s %s %s"
            ignores
            local-dir
            (string-remove-prefix "/ssh:" remote-dir))))

(defun -current-buffer-in-project()
  (in-any-project-p buffer-file-name))

(defun devremote-try-transfer-current ()
  (when (and devremote-mode (-current-buffer-in-project))
    (-copy-to-remote buffer-file-name)
    ))

(defun devremote-transfer-current ()
  (interactive)
  (if (-current-buffer-in-project)
      (devremote-try-transfer-current)
    (user-error "%s is not in any project" buffer-file-name)))


(defun devremote-find-file-hook-function()
  (when (-current-buffer-in-project)
    (devremote-mode)
    ))

(add-hook 'find-file-hook 'devremote-find-file-hook-function)

(define-minor-mode devremote-mode
  "Write code on home and test them on server by ssh"
  :lighter " DR"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f12>") 'devremote-transfer-current)
            (define-key map (kbd "C-c <f12>") 'devremote-transfer-current)
            map)
  )

(provide 'devremote)

;;; devremote.el ends here
