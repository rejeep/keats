(Then "^I should have one keat with key \"\\(.+\\)\" and description \"\\(.+\\)\"$"
      (lambda (key description)
        (should (equal 1 (length keats-list)))
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
