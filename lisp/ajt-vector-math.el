(defun ajt-quat-to-axis-angle (q)
  (let ((qx (car q))
        (qy (cadr q))
        (qz (caddr q))
        (qw (cadddr q)))
    (let ((angle (* 2.0 (acos qw)))
          (s (- 1.0 (* qw qw))))
      (if (<= s 0.0)
          (list 1.0 0.0 0.0 angle)
        (list (/ qx (sqrt s))
              (/ qy (sqrt s))
              (/ qz (sqrt s))
              angle)))))

;; TODO: make len/add/sub/dot work with lists of any length.
(defun ajt-vec3-len (v)
  (let ((vx (car v)) (vy (cadr v)) (vz (caddr v)))
    (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))

(defun ajt-vec3-add (v1 v2)
  (let ((v1x (car v1)) (v1y (cadr v1)) (v1z (caddr v1))
        (v2x (car v2)) (v2y (cadr v2)) (v2z (caddr v2)))
    (list (+ v1x v2x) (+ v1y v2y) (+ v1z v2z))))

(defun ajt-vec3-sub (v1 v2)
  (let ((v1x (car v1)) (v1y (cadr v1)) (v1z (caddr v1))
        (v2x (car v2)) (v2y (cadr v2)) (v2z (caddr v2)))
    (list (- v1x v2x) (- v1y v2y) (- v1z v2z))))

(defun ajt-lerp (a b alpha)
  (+ (* (- 1 alpha) a) (* alpha b)))


