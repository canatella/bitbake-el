;;; bitbake.el --- Running bitbake from emacs

;; Author: Damien Merenne
;; URL: https://github.com/canatella/bitbake-mode
;; Created: 2014-02-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (dash "2.6.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package integrate the bitbake development cycle into emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'mmm-mode)
(require 's)
(require 'dash)

;;; User customizable variables
(defgroup bitbake nil
  "Run bitbake commands from emacs"
  :group 'processes)

(defcustom bb-poky-directory "/opt/poky"
  "The default yocto poky directory."
  :type '(directory)
  :group 'bitbake)

(defcustom bb-build-directory ""
  "The default yocto build directory."
  :type '(directory)
  :group 'bitbake)

(defcustom bb-server-host nil
  "The name or IP address to use as host address of the server process.  If set, the server accepts remote connections; otherwise it is local."
  :type '(choice
          (string :tag "Name ro IP address")
          (const :tag "Local" nil))
  :group 'bitbake)

;;;###autoload
(put 'bb-server-host 'risky-local-variable t)

(defcustom bb-server-port nil
  "The port number that the server process should listen on.  A nil value means to use a random port number."
  :group 'bitbake
  :type '(choice
          (string :tag "Port number")
          (const :tag "Random" nil))
  :version "24.1")
;;;###autoload
(put 'server-port 'risky-local-variable t)

(defcustom bb-deploy-ssh-host nil
  "The host where to deploy package over ssh."
  :group 'bitbake
  :type '(string)
  :version "24.1")

(defcustom wic-definition-file nil
  "Path the wic definition file (wks file) to use for creating hdd image.

If a relative path is used, it will be relative to the poky directory."
  :group 'bitbake
  :type '(string)
  :version "24.1")

(defcustom bb-flash-device nil
  "The device file where to dd the hdd image.

If using a USB stick to boot the target platform, use a udev rule
to create a link to that disk and use the link name
here.  Calling bb-flash will copy the hdd image on the usb disk if present."
  :group 'bitbake
  :type '(string)
  :version "24.1")

;;; Local variables
(defvar bb-current-server-host nil "The actual host name or IP address of the bitbake server instance.")
(defvar bb-current-server-port nil "The actual port of the bitbake server instance.")
(defvar bb-current-poky-directory nil "The actual directory holding bitbake binaries.")
(defvar bb-current-build-directory nil "The actual build directory.")
(defvar bb-recipes-cache '() "Cache of bitbake recipes.")
(defvar bb-recipe-variables-cache '() "Cache of bitbake recipe variables.")
(defvar bb-recipe-tasks-cache '() "Cache of bitbake recipe variables.")
(defvar bb-recipe-history nil "History list of recipe names entered in the minibuffer.")
(defvar bb-image-history nil "History list of image names entered in the minibuffer.")
(defvar bb-task-history nil "History list of task names entered in the minibuffer.")
(defvar bb-task-queue nil "List of task left to execute.")
(defvar bb-current-task nil "The currently active task.")
(defvar bb-current-command nil "The running command.")
(defvar bb-last-disk-image nil "The last build disk image file.")
(defvar bb-buffer-prompt "/////---bitbake$ " "The prompt used in the bitbake buffer.")
(defvar bb-buffer-prompt-regexp (concat "^" (regexp-quote bb-buffer-prompt)) "A regexp matching the prompt")

(make-variable-buffer-local 'bb-last-disk-image)

;;; Minor mode functions

(defun bb-read-poky-directory ()
  "Read the poky directory."
  (read-directory-name "Poky directory" bb-poky-directory bb-poky-directory))

(defun bb-read-build-directory ()
  "Read the build directory."
  (read-directory-name "Build directory" bb-build-directory bb-build-directory))

(defun bb-wait-for-prompt ()
  "Wait for BB-BUFFER-PROMPT to appear in current buffer."
  (let ((bol))
    (save-excursion
      (beginning-of-line)
      (setq bol (point)))
    (while (not (search-backward bb-buffer-prompt bol t))
      (sleep-for 0 300))
    (goto-char (process-mark (get-buffer-process (current-buffer))))))

(defun bb-buffer ()
  "Fetch or create the bitbake buffer."
  (let ((buffer-name "*bitbake*")
        (default-directory bb-build-directory))
    (or (get-buffer buffer-name)
        (progn
          (let ((buffer (shell buffer-name)))
            (with-current-buffer buffer
              (setq-local comint-move-point-for-output t)
              (setq-local comint-prompt-regexp bb-buffer-prompt-regexp)
              (process-send-string (current-buffer) (format "export PS1='%s' && cd %s\n" bb-buffer-prompt default-directory))
              (bb-wait-for-prompt)
              (comint-send-input nil t)
              (erase-buffer)
              (bb-wait-for-prompt)
              buffer))))))

(defun bb-capture-buffer ()
  "Fetch or create the bitbake capture buffer."
  (get-buffer-create "*bitbake-temp*"))

(defun bb-add-path (path)
  "Add PATH to the current path."
  (let ((components (s-split ":" (or (getenv "PATH") "") t)))
    (unless (member path components)
      (setenv "PATH" (s-join ":" (-distinct (cons path components)))))))

(defun bb-remove-path (path)
  "Remove PATH from the current path."
  (let ((components (s-split ":" (or (getenv "PATH") "") t)))
    (when (member path components)
      (setenv "PATH" (s-join ":" (delete path components))))))

(defun bb-setup-environment (poky-directory build-directory)
  "Add POKY-DIRECTORY to PATH environment variable and set BBPATH to BUILD-DIRECTORY."
  (setenv "BBPATH" build-directory)
  (setenv "BUILDDIR" build-directory)
  (bb-add-path (format "%sscripts" poky-directory))
  (bb-add-path (format "%sbitbake/bin" poky-directory))
  (message "Bitbake: updated path to %s" (getenv "PATH")))

(defun bb-cleanup-environment (poky-directory)
  "Remove POKY-DIRECTORY from PATH environment variable and unset BBPATH."
  (setenv "BBPATH")
  (setenv "BUILDDIR")
  (bb-remove-path (format "%sscripts" poky-directory))
  (bb-remove-path (format "%sbitbake/bin" poky-directory)))

(defun bb-shell-command (command)
  "Run shell COMMAND in bitbake buffer.

Capture output in *bitbake-temp*."
  (with-current-buffer (bb-capture-buffer)
    (setq-local comint-prompt-regexp bb-buffer-prompt-regexp)
    (erase-buffer))
  (with-current-buffer (bb-buffer)
    (setq-local comint-prompt-regexp bb-buffer-prompt-regexp)
    (setq bb-current-command command)
    (comint-redirect-send-command (format "echo '%s' && %s 2>&1" command command) (bb-capture-buffer) t t)))

(defun bb-start-server (poky-directory build-directory)
  "Start a bitbake server instance.

Start a bitbake server using POKY-DIRECTORY to find the bitbake
binary and BUILD-DIRECTORY as the build directory."
  (interactive (let ((ask (consp current-prefix-arg)))
                 (list (if (or ask (not bb-poky-directory))
                           (bb-read-poky-directory)
                         bb-poky-directory)
                       (if (or ask (not bb-build-directory))
                           (bb-read-build-directory)
                         bb-build-directory))))

  ;; stop server if running
  (when (getenv "BBSERVER")
    (message "Stopping other instance of bitbake server")
    (bb-stop-server))

  ;; cleanup caches
  (setq bb-recipes-cache '()
        bb-recipe-variables-cache '()
        bb-recipe-tasks-cache '())

  ;; prepare environment
  (setq bb-current-server-host (or bb-server-host "localhost")
        bb-current-server-port (or bb-server-port (+ 1024 (random (- 65535 1024))))
        bb-current-poky-directory (file-name-as-directory poky-directory)
        bb-current-build-directory (file-name-as-directory build-directory))
  (bb-setup-environment  bb-current-poky-directory bb-current-build-directory)

  ;; start the server
  (shell-command (format "cd /tmp && bitbake -B %s:%s --server-only -t xmlrpc"
                         bb-current-server-host bb-current-server-port))

  (setenv "BBSERVER" (format "%s:%s" bb-current-server-host bb-current-server-port)))

(defun bb-stop-server ()
  "Stop the bitbake server instance."
  (interactive)
  (unless (getenv "BBSERVER")
    (user-error "No bitbake server running"))
  (message "Bitbake: stopping server")
  (call-process-shell-command (format "bitbake --kill-server --remote-server=%s" (getenv "BBSERVER")))
  (bb-cleanup-environment bb-current-poky-directory)
  (message "Bitbake: server is down, cleaning up")
  (setenv "BBSERVER")
  (setq bb-recipes-cache nil
        bb-recipe-variables-cache nil
        bb-recipe-tasks-cache nil
        bb-current-server-host nil
        bb-current-server-port nil
        bb-current-poky-directory nil
        bb-current-build-directory nil
        bb-current-task nil
        bb-task-queue nil)
  (setq-local kill-buffer-query-functions '())
  (let* ((buffer (get-buffer "*bitbake*"))
         (process (get-buffer-process buffer)))
    (when process (kill-process))
    (when buffer (kill-buffer buffer))))

(defun bb-parse-recipes (buffer)
  "Parse recipes in a BUFFER given EVENT."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((recipes))
      (while (re-search-forward "^\\([^[:blank:]]+\\) +\\([[:digit:]]*\\):\\([^[:blank:]]+\\)-\\([^[:blank:]]+\\)" nil t)
          (setq recipes (cons (list (match-string 1) (match-string 2) (match-string 3) (match-string 4)) recipes)))
        recipes)))

(defun bb-s-command (command message)
  "Run COMMAND synchronously, sending output to bb-capture-buffer.

If COMMAND fails, raise user error with MESSAGE."
  (unless (zerop (shell-command command (bb-capture-buffer)))
    (user-error "%s: %s" message
                (if (getenv "BBSERVER")
                    (buffer-string) "server is not started"))))

(defun bb-fetch-recipes ()
  "Fetch the availables bitbake recipes for the POKY-DIRECTORY and the BUILD-DIRECTORY."
  (message "Bitbake: fetching recipes")
  (with-current-buffer (bb-capture-buffer)
    (bb-s-command "bitbake -s 2>&1" "Unable to fetch recipes")
    (bb-parse-recipes (current-buffer))))

(defun bb-recipes (&optional fetch)
  "Return the bitbake recipes list.

If FETCH is non-nil, invalidate cache and fetch the recipes list again."
  (when (or fetch (not bb-recipes-cache))
    (setq bb-recipes-cache (bb-fetch-recipes)))
  bb-recipes-cache)

(defun bb-recipe-info (recipe)
  "Return the recipe list associated to RECIPE."
  (or (assoc recipe (bb-recipes)) (user-error "Unknown recipe %s", recipe)))

(defun bb-recipe-names ()
  "Return the list of available recipe names."
  (let (names)
    (dolist (recipe (bb-recipes) names)
      (setq names (cons (car recipe) names)))))

(defun bb-buffer-recipe (&optional buffer)
  "Return a recipe name for the current buffer or BUFFER if given."
  (setq buffer (or buffer (current-buffer)))
  (when (stringp (buffer-file-name buffer))
    (let ((bb-directory (locate-dominating-file (buffer-file-name buffer)
                                                (lambda (dir)
                                                  (and (file-directory-p dir)
                                                       (directory-files dir t "\\.bb\\(append\\)?\\'"))))))
      (when bb-directory
        (let ((bb-file (car (directory-files bb-directory t "[^/_]+\\(_[^_]+\\)?\\.bb\\(append\\)?\\'"))))
          (with-temp-buffer
            (goto-char (point-min))
            (insert bb-file)
            (goto-char (point-min))
            (if (re-search-forward "\\([^/_]+\\)\\(_[^_]+\\)?\\.bb\\(append\\)?\\'" nil t)
                (match-string 1))))))))

(defun bb-read-recipe ()
  "Read a recipe name in the minibuffer, with completion."
  (let ((default (or (bb-buffer-recipe) (car (last bb-recipe-history)))))
    (completing-read "Recipe: " (bb-recipe-names) nil t default 'bb-recipe-history)))

(defun bb-image-names ()
  "Return the list of available image names."
  (let (names)
    (dolist (recipe (bb-recipes) names)
      (if (string-match ".*-image\\(-.*\\|$\\)" (car recipe))
          (setq names (cons (car recipe) names))))))

(defun bb-read-image ()
  "Read a image name in the minibuffer, with completion."
  (completing-read "Image: " (bb-image-names) nil t (car (last bb-image-history)) 'bb-image-history))

(defun bb-parse-recipe-tasks (buffer)
  "Parse the list of recipe tasks in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward ".*ERROR.*: \\(\x1b\\[..?m\\)?\\(.*\\)\\(\x1b\\[..?m\\)?" nil t)
          (error (format "Bitbake: unable to fetch tasks - %s" (match-string 2))))
    (let ((tasks))
      (while (re-search-forward "^do_\\([^[:blank:]\n]+\\)" nil t)
        (setq tasks (cons (match-string 1) tasks)))
      tasks)))

(defun bb-fetch-recipe-tasks (recipe)
  "Fetch the list of bitbake tasks for RECIPE."
  (message "Bittbake: fetching recipe %s tasks" recipe)
  (with-current-buffer (bb-capture-buffer)
    (shell-command (format "bitbake %s -c listtasks" recipe) (bb-capture-buffer))
    (bb-parse-recipe-tasks (current-buffer))))

(defun bb-recipe-tasks (recipe &optional fetch)
  "Return the bitbake tasks for RECIPE.

If FETCH is non-nil, invalidate cache and fetch the tasks again."
  (when (or fetch (not (assoc recipe bb-recipe-tasks-cache)))
    (assq-delete-all recipe bb-recipe-tasks-cache)
    (setq bb-recipe-tasks-cache (cons (list recipe (bb-fetch-recipe-tasks recipe)) bb-recipe-tasks-cache)))
  (cadr (assoc recipe bb-recipe-tasks-cache)))

(defun bb-read-tasks (recipe)
  "Read a task name in the minibuffer, with completion for task RECIPE."
  (let ((tasks (bb-recipe-tasks recipe)))
    (completing-read "Task: " tasks nil t (car (last bb-task-history)) 'bb-task-history)))

(defun bb-parse-recipe-variables (buffer)
  "Parse bitbake variables BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^[[:alnum:]_]+()[[:space:]]+{")
    (beginning-of-line)
    (let ((limit (point))
          (variables))
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:alnum:]~+.${}/_-]+\\)=\"\\([^\"]*\\)" limit t)
        (let ((name (substring-no-properties (match-string 1)))
              (value (substring-no-properties (match-string 2))))
          (while (equal (string (char-before)) "\\")
            (re-search-forward "\\(\"[^\"]*\\)" limit t)
            (setq value (concat (substring value 0 -1) (substring-no-properties (match-string 1)))))
          (setq variables (cons (cons name value) variables))))
      variables)))

(defun bb-fetch-recipe-variables (recipe)
  "Fetch bitbake variables for RECIPE."
    (message "Bittbake: fetching recipe %s variables" recipe)
  (with-temp-buffer
    (shell-command (format "bitbake -e %s 2>&1" recipe) (current-buffer))
    (bb-parse-recipe-variables (current-buffer))))

(defun bb-recipe-variables (recipe &optional fetch)
  "Return the bitbake variables for RECIPE.

If FETCH is non-nil, invalidate cache and fetch the variables again."
  (when (or fetch (not (assoc recipe bb-recipe-variables-cache)))
    (assq-delete-all recipe bb-recipe-variables-cache)
    (setq bb-recipe-variables-cache (cons (list recipe (bb-fetch-recipe-variables recipe)) bb-recipe-variables-cache)))
  (cadr (assoc recipe bb-recipe-variables-cache)))

(defun bb-recipe-variable (variable recipe &optional fetch)
  "Return the value of VARIABLE for RECIPE.

If FETCH is non-nil, invalidate cache and fetch the variables again."
  (cdr (assoc variable (bb-recipe-variables recipe fetch))))

(defun bb-uuid ()
  "Generate a random UUID."
  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6))))

(defun bb-recipe-taint-task (recipe task)
  "Taint RECIPE TASK as a workarround for bitbake -f not working in server mode."
  (let ((taint-file-name (format "%s.do_%s.taint" (bb-recipe-variable "STAMP" recipe) task)))
    (with-temp-file taint-file-name
      (insert (bb-uuid)))))

(defun bb-schedule-queue ()
  "Schedule the next task in queue."
  (when bb-current-command
    (message "Bitbake: command \"%s\" finished" bb-current-command)
    (with-current-buffer (bb-capture-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^[| ]*ERROR: \\([^(]+\\)\\( (log file is located at \\(.*\\))\\)?$" nil t)
        (message "Bitbake: error - %s" (match-string 1))
        (let ((log-file (match-string 3)))
          (if log-file
              (find-file log-file)))))
    (setq bb-current-command nil))
  (message "Bitbake: scheduling queue")
  (with-current-buffer (bb-buffer)
    (let ((task (car bb-task-queue)))
      (when task
        (setq bb-task-queue (cdr bb-task-queue)
              bb-current-task task)
        (add-hook 'comint-redirect-hook 'bb-schedule-queue nil t)
        (message "Bitbake: running task")
        (run-at-time 0 nil task))
      (unless task
        (message "Bitbake: task queue empty")
        (setq bb-current-task nil)
        (remove-hook 'comint-redirect-hook 'bb-schedule-queue t)
        ))))

(defmacro bb-command (varlist &rest body)
  "Create a command with VARLIST to execute BODY and put it in the queue."
  (declare (indent 1))
  `(bb-queue-command
    (lexical-let ,(mapcar (lambda (var)
                            (list var var))
                          varlist)
      (lambda ()
        (with-current-buffer (bb-buffer)
          (condition-case err
              (progn
                (setq bb-current-command nil)
                (progn ,@body)
                (unless bb-current-command
                    (bb-schedule-queue)))
            (error (bb-reset-queue)
                   (message "Bitbake: error - %s." (error-message-string err)))))))))

(defun bb-run-queue ()
  "Maybe run next process in queue if no other task is active."
  (unless bb-current-task
    (message "Bitbake: no running tasks, scheduling queue")
    (bb-schedule-queue)))

(defun bb-queue-command (task)
  "Queue TASK for running in bitbake buffer."
  (message "Bitbake: queuing command")
  (setq bb-task-queue (append bb-task-queue (list task)))
  (bb-run-queue))

(defun bb-reset-queue ()
  "Reset task queue."
  (interactive)
  (setq bb-task-queue nil
        bb-current-task nil))

(defun bb-task (task recipe &optional force)
  "Run bitbake TASK on RECIPE.

If FORCE is non-nil, force running the task."
  (interactive (let* ((recipe (bb-read-recipe))
                      (task (bb-read-tasks recipe)))
                 (list task recipe (consp current-prefix-arg))))
  (bb-command (recipe task force)
    (when force
      (bb-recipe-taint-task recipe task))
    (bb-shell-command (format "bitbake %s %s -c %s" recipe (if force "-f" "") task))))

(defun bb-recipe (recipe)
  "Run bitbake RECIPE."
  (interactive (list (bb-read-recipe)))
  (bb-command (recipe)
    (bb-shell-command (format "bitbake %s " recipe))))

(defun bb-clean (recipe)
  "Run bitbake clean on RECIPE."
  (interactive (list (bb-read-recipe)))
  (bb-task "clean" recipe t))

(defun bb-compile (recipe)
  "Run bitbake compile on RECIPE."
  (interactive (list (bb-read-recipe)))
  (bb-task "compile" recipe t))

(defun bb-install (recipe)
  "Run bitbake install on RECIPE."
  (interactive (list (bb-read-recipe)))
  (bb-task "install" recipe t))

(defun bb-fetch (recipe)
  "Run bitbake install on RECIPE."
  (interactive (list (bb-read-recipe)))
  (bb-task "fetch" recipe t))

(defun bb-recompile (recipe)
  "Run bitbake clean compile and install on RECIPE."
  (interactive (list (bb-read-recipe)))
  (bb-task "cleanall" recipe)
  (bb-task "fetch" recipe)
  (bb-recipe recipe))

(defun bb-deploy (recipe)
  "Deploy artifacts of RECIPE to bb-deploy-ssh host."
  (interactive (list (bb-read-recipe)))
  (bb-command (recipe)
    (let ((image (bb-recipe-variable "D" recipe)))
      (message "Duma: deploying %s" recipe)
      (bb-shell-command (format "tar -C %s -cf - . | ssh %s tar -C / -xf -" image bb-deploy-ssh-host)))))

(defun bb-recompile-deploy (recipe)
  "Recompile RECIPE and deploy its artifacts."
  (interactive (list (bb-read-recipe)))
  (bb-recompile recipe)
  (bb-deploy recipe))

(defun bb-image (image &optional force)
  "Run bitbake IMAGE.

If FORCE is non-nil, force rebuild of image,"
  (interactive (list (bb-read-image) (consp current-prefix-arg)))
  (bb-command (image force)
    (when force
      (bb-recipe-taint-task image "rootfs"))
    (bb-shell-command (format "bitbake %s" image))))

(defun bb-workdir (recipe)
  "Open RECIPE workdir."
  (interactive (list (bb-read-recipe)))
  (find-file (bb-recipe-variable "WORKDIR" recipe)))

(defun bb-file (recipe)
  "Open RECIPE bitbake file."
  (interactive (list (bb-read-recipe)))
  (find-file (bb-recipe-variable "FILE" recipe)))

(defun bb-rootfs (image)
  "Open IMAGE root fs."
  (interactive (list (bb-read-image)))
  (find-file (bb-recipe-variable "IMAGE_ROOTFS" image)))

(defun wic-read-definition-file ()
  "Read path to a wic wks definition file."
  (if wic-definition-file
      (if (file-name-absolute-p wic-definition-file) wic-definition-file
        (format "%s%s" bb-current-poky-directory wic-definition-file))
    (read-file-name "Definition file: " bb-current-poky-directory nil t nil
                    (lambda (name)
                      (or (file-directory-p name)
                          (string-match "\\.wks\\'" name))))))

(defun wic-create (wks image)
  "Run wic WKS -e IMAGE."
  (interactive (list (wic-read-definition-file)
                     (bb-read-image)))
  (let ((rootfs (bb-recipe-variable "IMAGE_ROOTFS" image))
        (kernel (bb-recipe-variable "STAGING_KERNEL_DIR" image))
        (hdddir (bb-recipe-variable "HDDDIR" image))
        (staging-data (bb-recipe-variable "STAGING_DATADIR" image))
        (native-sysroot (bb-recipe-variable "STAGING_DIR_NATIVE" image))
        (deploy (bb-recipe-variable "DEPLOY_DIR_IMAGE" image))
        (last-prompt (process-mark (get-buffer-process (bb-buffer)))))
    (bb-command (wks rootfs staging-data kernel native-sysroot deploy)
      (bb-shell-command (format "wic create %s -r %s -b %s -k %s -n %s -o %s"
                                wks rootfs staging-data kernel native-sysroot deploy)))
    (bb-command ()
      (let (disk-image)
        (with-current-buffer (bb-capture-buffer)
          (goto-char (point-min))
          (unless (re-search-forward "The new image(s) can be found here:\n *\\(.*\\)" nil t)
            (error "Unable to execute wic command, see *bitbake* for details"))
          (setq disk-image (match-string 1)))
        (message "Disk image %s created" disk-image)
        (setq bb-last-disk-image disk-image)))))

(defun bb-hdd-image (wks image)
  "Create an hdd image using wic based on WKS definition file and bitbake IMAGE."
  (interactive (list (wic-read-definition-file)
                     (bb-read-image)))
  (bb-image image)
  (wic-create wks image))

(defun bb-flash-image (wks image)
  "Create an hdd image using wic and flash it on bb-flash-device.

The hdd image is based on WKS definition file and bitbake IMAGE, see bb-hdd-image."
  (interactive (list (wic-read-definition-file)
                     (bb-read-image)))
  (bb-hdd-image wks image)
  (bb-command ()
    (when (and (file-exists-p bb-flash-device) bb-last-disk-image)
      (message "Bitbake: copy image to %s" bb-flash-device)
      (bb-shell-command (format "dd if=%s of=%s bs=32M" bb-last-disk-image bb-flash-device)))))

;;; Mode definition
(defvar bitbake-minor-mode-map nil "Keymap for bitbake-mode.")

(setq bitbake-minor-mode-map nil)
(when (not bitbake-minor-mode-map)
  (setq bitbake-minor-mode-map (make-sparse-keymap))
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ s") 'bb-start-server)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ k") 'bb-stop-server)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ r") 'bb-recompile)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ d") 'bb-recompile-deploy)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ f") 'bb-flash-image)
  (define-key bitbake-minor-mode-map (kbd "C-c C-/ t") 'bb-task)
  (easy-menu-define bitbake-menu bitbake-minor-mode-map
    "BitBake"
    '("BitBake"
      ["Start server"   bb-start-server]
      ["Stop server"    bb-stop-server ]
      ["Recipe"         bb-recipe]
      ["Task"           bb-task]
      ("Tasks"
       ["clean"         bb-clean]
       ["compile"       bb-compile]
       ["install"       bb-install]
       ["fetch"         bb-fetch]
       ["recompile"     bb-recompile]
       ["deploy"        bb-deploy]
       ["recompile, deploy" bb-recompile-deploy])
      ("Image"
       ["build"         bb-image]
       ["wic"           wic-create]
       ["hdd"           bb-hdd-image]
       ["flash"         bb-flash-image]))))

(define-minor-mode bitbake-minor-mode
  "Toggle Bitake mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :init-value nil
  :lighter nil
  :keymap bitbake-minor-mode-map
  :group 'bitbake
  :global t)

(defun bb-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.

For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defvar bitbake-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table used in `bitbake-mode'.")

(defvar bb-font-lock-defaults
  `((
     ("include\\|require\\|inherit\\|python\\|addtask" . font-lock-keyword-face)
     ("do_\\(\\s_\\|\\sw\\)+" . font-lock-function-name-face)
     )))

(defun bb-indent-line ()
  "Indent current line as bitbake code."
  (interactive)
  (beginning-of-line)
  (if (looking-back "\\\\\n")
      (indent-line-to default-tab-width)
    (indent-line-to 0)))

(define-derived-mode bitbake-mode fundamental-mode
  "A mode for editing bitbake recipe files."
  :syntax-table bitbake-syntax-table
  (setq font-lock-defaults bb-font-lock-defaults)
  (setq mode-name "BitBake")
  (set (make-local-variable 'indent-line-function) 'bb-indent-line)
  (define-key bitbake-mode-map [remap comment-dwim] 'bb-comment-dwim))

(mmm-add-classes
 '((bitbake-shell
    :submode shell-script-mode
    :delimiter-mode nil
    :case-fold-search nil
    :front "do_\\(\\s_\\|\\sw\\)+(\\s-*)\\s-*{"
    :back "}")
   (bitbake-python
    :submode python-mode
    :delimiter-mode nil
    :case-fold-search nil
    :front "python\\s-+\\(\\s_\\|\\sw\\)+\\s-*{"
    :back "}")))

(mmm-add-mode-ext-class 'bitbake-mode "\\.bb\\'" 'bitbake-shell)
(mmm-add-mode-ext-class 'bitbake-mode "\\.bb\\'" 'bitbake-python)
(setq auto-mode-alist
         (cons '("\\.bb\\(append\\)?\\'" . bitbake-mode) auto-mode-alist))

(provide 'bitbake)

;;; bitbake.el ends here
