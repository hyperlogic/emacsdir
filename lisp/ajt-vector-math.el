(require 'cl-lib)

;; quaternions are expected to be a list of four elements
;; '(x y z w), w is the real part of the quaternion and
;; x y z are the i j k imaginary elements

(defun ajt-quat-to-axis-angle (q)
  (let ((qx (car q)) (qy (cadr q)) (qz (caddr q)) (qw (cadddr q)))
    (let ((angle (* 2.0 (acos qw)))
          (s (- 1.0 (* qw qw))))
      (if (<= s 0.0)
          (list 1.0 0.0 0.0 angle)
        (list (/ qx (sqrt s))
              (/ qy (sqrt s))
              (/ qz (sqrt s))
              angle)))))

(defun ajt-axis-angle-to-quat (axis angle)
  (let ((x (car axis))
        (y (cadr axis))
        (z (caddr axis))
        (sin-half-angle (sin (/ angle 2.0)))
        (cos-half-angle (cos (/ angle 2.0))))
    (list (* x sin-half-angle) (* y sin-half-angle) (* z sin-half-angle) cos-half-angle)))

(defun ajt-quat-mul (a b)
  (let ((a_2 (car a)) (a_3 (cadr a)) (a_4 (caddr a)) (a_1 (cadddr a))
        (b_2 (car b)) (b_3 (cadr b)) (b_4 (caddr b)) (b_1 (cadddr b)))
    (list (+ (* a_1 b_2) (* a_2 b_1) (* a_3 b_4) (- (* a_4 b_3)))
          (+ (* a_1 b_3) (- (* a_2 b_4)) (* a_3 b_1) (* a_4 b_2))
          (+ (* a_1 b_4) (* a_2 b_3) (- (* a_3 b_2)) (* a_4 b_1))
          (+ (* a_1 b_1) (- (* a_2 b_2)) (- (* a_3 b_3)) (- (* a_4 b_4))))))

(defun ajt-quat-inv (q)
  (let ((qx (car q)) (qy (cadr q)) (qz (caddr q)) (qw (cadddr q)))
    (list qx qy qz (- qw))))

(defun ajt-quat-conj (q)
  (let ((qx (car q)) (qy (cadr q)) (qz (caddr q)) (qw (cadddr q)))
    (list (- qx) (- qy) (- qz) qw)))

;; rotate vector v by q
(defun ajt-rotate (q v)
  (let ((qv (list 0.0 (car v) (cadr v) (caddr v))))
    (ajt-quat-mul (ajt-quat-mul q qv) (ajt-quat-conj q))))

(defun ajt-vec-dot (a b)
  (cl-reduce '+ (mapcar* '* a b)))

(defun ajt-vec-len (v)
  (sqrt (ajt-vec-dot v v)))

(defun ajt-vec-add (a b)
  (mapcar* '+ a b))

(defun ajt-vec3-sub (a b)
  (mapcar* '- a b))

(defun ajt-vec-mul (a b)
  (mapcar* '* a b))

(defun ajt-vec-x (v)
  (car v))

(defun ajt-vec-y (v)
  (cadr v))

(defun ajt-vec-z (v)
  (caddr v))

(defun ajt-vec-w (v)
  (cadddr v))

(defun ajt-vec3-normalize (v)
  (let ((len (/ 1.0 (ajt-vec-len v))))
    (ajt-vec-mul (list len len len) v)))

(defun ajt-quat-normalize (v)
  (let ((len (/ 1.0 (ajt-vec-len v))))
    (ajt-vec-mul (list len len len len) v)))

(defun ajt-lerp (a b alpha)
  (+ (* (- 1 alpha) a) (* alpha b)))

(defun ajt-rad-to-deg (a)
  (* (/ 180 pi) a))

(defun ajt-deg-to-rad (a)
  (* (/ pi 180) a))

(defun ajt-vec3-cross (a b)
  (let ((ax (car a)) (ay (cadr a)) (az (caddr a))
        (bx (car b)) (by (cadr b)) (bz (caddr b)))
    (list (- (* ay bz) (* az by))
          (- (* az bx) (* ax bz))
          (- (* ax by) (* ay bx)))))
