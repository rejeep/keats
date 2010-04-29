(Then "^I should have a keat with key \"\\(.+\\)\" and description \"\\(.+\\)\"$"
      (lambda (key description)
        (let ((keat (car keats-list)))
          (should (equal key (keats-keat-key keat)))
          (should (equal description (keats-keat-description keat))))))

(Then "^I should have \\([0-9]+\\) keats?$"
      (lambda (count)
        (should (equal (string-to-int count) (length keats-list)))))

(Given "^I \\(enable\\|disable\\) keats-mode$"
       (lambda (status)
         (if (string= status "enable")
             (keats-mode 1)
           (keats-mode -1))))

(Then "^the prefix should be \\(enabled\\|disabled\\)$"
      (lambda (status)
        (if (string= status "enabled")
            (should (key-binding (read-kbd-macro "C-c k"))))))

(Given "^I have one keat with key \"\\(.+\\)\" and description \"\\(.+\\)\"$"
       (lambda (key description)
         (add-to-list 'keats-list (make-keats-keat :key key :description description) t)))

(Then "^I should have \\([0-9]+\\) keats?$"
      (lambda (count)
        (should (equal (string-to-number count) (length keats-list)))))

(Then "^I should have these keats:$"
      (lambda (keats)
        (should (equal (length (cdr keats)) (length keats-list)))
        (let (key description)
          (dolist (keat (cdr keats))
            (setq key (car keat))
            (setq description (cadr keat))
            (should
             (some (lambda (k)
                     (and
                      (equal (keats-keat-key k) key)
                      (equal (keats-keat-description k) description)))
                   keats-list))))))

(Given "^the keats-file is set to a non existing file$"
       (lambda ()
         (setq keats-file (make-temp-file "keats"))
         ))

(When "^I load this keats file:$"
      (lambda (contents)
        (with-temp-file keats-file
          (insert contents))
        (keats-load)))

(When "^I load the keats file$"
      (lambda ()
        (keats-load)))

(Then "^I should have a keats file$"
      (lambda ()
        (should (file-exists-p keats-file))))

(Given "^I have this keats file:$"
       (lambda (contents)
         (with-temp-file keats-file
           (insert contents))))

(Then "^the keats file should look like this:$"
      (lambda (contents)
        (with-temp-buffer
          (insert-file-contents-literally keats-file)
          (should (equal (buffer-substring-no-properties (point-min) (point-max)) contents)))))

