(ert-deftest terminating-keys ()
  (should (keats-terminating-key-p [13])) ;; RET
  (should (keats-terminating-key-p [7]))) ;; C-g

(ert-deftest non-terminating-keys ()
  (should-not (keats-terminating-key-p [16]))    ;; C-p
  (should-not (keats-terminating-key-p [3 13]))) ;; C-c RET

(ert-deftest prefix-key ()
  (should (equal "C-c k" keats-prefix-key)))

(ert-deftest read-description ()
  (with-mock
   (stub read-string => "description")
   (should (equal "description" (keats-read-description)))))

(ert-deftest find-keat-does-not-exist ()
  (preserve-keats
   (keats-add (make-keats-keat :key "C-x b"))
   (should-not (keats-find "C-x C-b"))))

(ert-deftest find-keat-does-exist ()
  (preserve-keats
   (keats-add (make-keats-keat :key "C-x b"))
   (let ((keat (keats-find "C-x b")))
     (should keat)
     (should (keats-keat-p keat)))))

(ert-deftest update-keat ()
  (let ((keat (make-keats-keat :key "C-x b" :description "old")))
    (keats-update keat "new")
    (should (equal "new" (keats-keat-description keat)))))

(ert-deftest get-existing-key ()
  (preserve-keats
   (with-mock
    (stub keats-read-key => "C-x b")
    (let ((keat (make-keats-keat :key "C-x b")))
      (keats-add keat)
      (keats-get
       (should (keats-keat-p keat)))))))

(ert-deftest get-non-existing-key ()
  (with-mock
   (stub keats-read-key => "C-x b")
   (keats-get
    (should-not keat))))


(defmacro preserve-keats (&rest body)
  "Executes body without messing with `keats-list'."
  `(let ((keats-list))
     ,@body))

(defun keats-add (keat)
  "Adds KEAT to `keats-list'."
  (add-to-list 'keats-list keat))
