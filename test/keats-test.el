(ert-deftest terminating-keys ()
  (should (terminating-key-p [13])) ;; RET
  (should (terminating-key-p [7]))) ;; C-g

(ert-deftest non-terminating-keys ()
  (should-not (terminating-key-p [16]))    ;; C-p
  (should-not (terminating-key-p [3 13]))) ;; C-c RET

(ert-deftest valid-keats ()
  (let ((keat (make-keats-keat :key "C-x b" :description "Valid")))
    (should (keats-valid-keat-p keat))))

(ert-deftest invalid-keats ()
  (let ((keats
         (list
          (make-keats-keat :key ""      :description "Valid")
          (make-keats-keat :key "C-x b" :description ""))))
    (dolist (keat keats)
      (should-not (keats-valid-keat-p keat)))))
