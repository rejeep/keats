(Then "^I should have one keat with key \"\\(.+\\)\" and description \"\\(.+\\)\"$"
      (lambda (key description)
        (should (equal 1 (length keats-list)))
        (let ((keat (car keats-list)))
          (should (equal key (keats-keat-key keat)))
          (should (equal description (keats-keat-description keat))))))

(Then "^I should have \\([0-9]+\\) keats?$"
      (lambda (count)
        (should (equal (string-to-int count) (length keats-list)))))
