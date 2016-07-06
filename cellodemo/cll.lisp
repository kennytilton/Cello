(in-package :cl-user)

(defparameter *iteration-count-inner* 0
  "keeps track of (fixed-source) flux iterations to measure convergence speed")
(defparameter *iteration-count-outer* 0
  "keeps track of source iterations to measure convergence speed")
(defparameter *iteration-count-mesh* 0
  "keeps track of number of mesh refinements to measure convergence speed")
(defparameter *iteration-count-length* 0
  "keeps track of number of length guesses to measure convergence speed")
(defparameter *flux* (make-array 3))

(defparameter *D* 0.9
  "neutron diffusion coefficient (cm)")
(defparameter *sigma-a* 0.066
  "absorption cross section (1/cm)")
(defparameter *v-sigma-f* 0.07
  "fission cross section times neutrons per fission (1/cm)")
(defparameter *k-tolerance* 1e-5
  "acceptable uncertainty of criticality constant")
(defparameter *flux-tolerance* 1e-3
  "acceptable uncertainty of flux")
(defparameter *left-albedo* 0
  "ratio of return current to outgoing current at left edge")
(defparameter *right-albedo* 0
  "ratio of return current to outgoing current at right edge")

;I copied this function from Vladimir Zolotykh, 
;who in turn copied it from Barry Margolin.
(defun copy-array (array) 
  (let ((new (make-array (array-dimensions array) :displaced-to array))) 
    (adjust-array new (array-dimensions array) :displaced-to nil))) 

(defun phi-center (phiE phiW source dx D sigma-a)
  (/
    (+ (/ (+ phiW phiE) dx dx) (/ source D))
    (+ (/ 2 dx dx) (/ sigma-a D)))) 

(defun phi-edge (phiOpp albedo source dx D sigma-a)
  (/
    (+ (/ (* 2 phiOpp) dx dx) (/ source D))
    (+
      (/ 2 dx dx)
      (/ (- 1 albedo) (* (+ 1 albedo) D dx))
      (/ sigma-a D))))

(defun multiply-array-scalar (inputarray scalar &aux resultarray)
  (setf resultarray (copy-array inputarray))
  ;this dotimes pattern based on Steele p452
  (dotimes (i (array-total-size resultarray))
    (setf (row-major-aref resultarray i) 
      (* scalar (row-major-aref resultarray i))))
  resultarray)

(defun set-source (flux v-sigma-f k)
  (multiply-array-scalar flux (* v-sigma-f (/ k))))

(defun gauss-seidel (source flux dx D sigma-a albedoL albedoR
                      &aux flux-old flux-sum flux-old-sum)
  (do () (nil)
    (incf *iteration-count-inner*)
    (setf flux-old (copy-array flux))
    (setf (aref flux 0) 
      (phi-edge (aref flux 1) albedoL (aref source 0) dx D sigma-a))
    (setf (aref flux (1- (length flux)))
      (phi-edge (aref flux (- (length flux) 2))
        albedoR (aref source (1- (length flux))) dx D sigma-a))
    (do ((i 1 (1+ i)))
        ((>= i (- (length flux) 1)))

      (setf (aref flux i) 
        (phi-center 
          (aref flux (1+ i)) (aref flux (1- i)) 
          (aref source i) dx D sigma-a)))
    (setf flux-sum (loop for x across flux sum x))
    (setf flux-old-sum (loop for x across flux-old sum x))
    (when (< (abs (- flux-sum flux-old-sum)) (* flux-sum *flux-tolerance*))
      (return-from gauss-seidel flux))))

(defun jacobian (source flux dx D sigma-a albedoL albedoR
                  &aux flux-old flux-sum flux-old-sum)
  (do () (nil)
    (incf *iteration-count-inner*)
    (setf flux-old (copy-array flux))
    (setf (aref flux 0) 
      (phi-edge (aref flux-old 1) albedoL (aref source 0) dx D sigma-a))
    (setf (aref flux (1- (length flux)))
      (phi-edge (aref flux-old (- (length flux) 2))
        albedoR (aref source (1- (length flux))) dx D sigma-a))
    (do ((i 1 (1+ i)))
        ((>= i (- (length flux) 1)))
      (setf (aref flux i) 
        (phi-center 
          (aref flux-old (1+ i)) (aref flux-old (1- i)) 
          (aref source i) dx D sigma-a)))
    (setf flux-sum (loop for x across flux sum x))
    (setf flux-old-sum (loop for x across flux-old sum x))
    (when (< (abs (- flux-sum flux-old-sum)) (* flux-sum *flux-tolerance*))
      (return-from jacobian flux))))

(defun criticality-fixed (flux-solver flux dx D v-sigma-f sigma-a albedoL albedoR
                           &aux k k-old flux-sum source)
  (setf k 1)
  (setf flux-sum (loop for x across flux sum x))
  (do () (nil)
    (incf *iteration-count-outer*)
    ;normalize flux to prevent underflow
    (setf flux (multiply-array-scalar flux (/ flux-sum)))
    (setf source (set-source flux v-sigma-f k))
    (funcall flux-solver source flux dx D sigma-a albedoL albedoR)
    (setf flux-sum (loop for x across flux sum x))
    (setf k-old k)
    (setf k (* k-old flux-sum))
    (when (< (abs (- k k-old)) (* k *k-tolerance*))
      (return-from criticality-fixed k))))

(defun expand-flux (flux dx D sigma-a v-sigma-f &aux result-flux)
  (declare (ignorable v-sigma-f sigma-a d dx))
  (setf result-flux (make-array (1- (* 2 (length flux))) :initial-element 1))
  (dotimes (i (array-total-size result-flux))
    (setf (aref result-flux i) 
      (if (evenp i) 
        (aref flux (/ i 2))
        (* 0.5 (+ (aref flux (/ (1- i) 2)) (aref flux (/ (1+ i) 2))))
      )))
  result-flux
  )

(defun criticality-refine (flux-solver slab-length D v-sigma-f sigma-a albedoL albedoR
                            &aux k flux dx k-old)
  (setf k 0)
  (setf flux (make-array 3 :initial-element 1))
  (do () (nil)
    (incf *iteration-count-mesh*)
    (setf dx (/ slab-length (1- (length flux))))
    (setf k-old k)
    (setf k (criticality-fixed flux-solver flux dx D v-sigma-f sigma-a albedoL albedoR))
    (setf *flux* flux)
    (when (< (abs (- k k-old)) (* k *k-tolerance*))
      (return-from criticality-refine k))
    (setf flux (expand-flux flux (/ dx 2) D sigma-a v-sigma-f))
  )
)

(defun criticality-search (flux-solver D v-sigma-f sigma-a albedoL albedoR
                            &aux LOW  K-LOW HIGH K-HIGH SLAB-LENGTH K)
  (defparameter *iteration-count-inner* 0
    "keeps track of (fixed source) flux iterations to measure convergence speed")
  (defparameter *iteration-count-outer* 0
    "keeps track of source iterations to measure convergence speed")
  (defparameter *iteration-count-mesh* 0
    "keeps track of number of mesh refinements to measure convergence speed")
  (defparameter *iteration-count-length* 0
    "keeps track of number of length guesses to measure convergence speed")
  (setf low 0)
  (setf k-low 0)
  (setf high nil)
  (setf k-high nil)
  (setf slab-length 1)
  (do () (nil)
    (incf *iteration-count-length*)
    (setf k (criticality-refine flux-solver slab-length D v-sigma-f sigma-a albedoL albedoR))
    (when (< k 1) 
      (setf k-low k)
      (setf low slab-length))
    (when (> k 1) 
      (setf k-high k)
      (setf high slab-length))
    (when (null high)
      (setf slab-length (* 2 slab-length)))
    (when (numberp high)
      (setf slab-length (* 0.5 (+ low high))))
    (when (numberp high)
      (when (< (- k-high k-low) *k-tolerance*)
        (return-from criticality-search (* 0.5 (+ low high)))))))

; this is the destruction operator
; represented in Duderstadt by matrix M
; was meant to be used in direct method
(defun set-destruction (nodes dx D sigma-a albedoL albedoR
                         &aux (M (make-array (list nodes nodes) :initial-element 0)))
  ;set diagonal
  (dotimes (i nodes)
    (setf 
      (aref M i i)
      (+ sigma-a (* 2 D (/ dx) (/ dx)))))
  ;set next to diagonals (except first and last rows)
  (dotimes (i (- nodes 2))
    (setf
      (aref M (1+ i) i)
      (* -1 D (/ dx) (/ dx)))
    (setf 
      (aref M (+ i 1) (+ i 2))
      (* -1 D (/ dx) (/ dx))))
  ;add albedo to corners
  (setf
    (aref M 0 0)
    (+ 
      (aref M 0 0)
      (/ (- 1 albedoL) dx (+ 1 albedoL))))
  (setf
    (aref M (1- nodes) (1- nodes))
    (+ 
      (aref M (1- nodes) (1- nodes))
      (/ (- 1 albedoR) dx (+ 1 albedoR))))
  ;set next to corners
  (setf
    (aref M 0 1)
    (* -2 D (/ dx) (/ dx)))
  (setf 
    (aref M (- nodes 1) (- nodes 2))
    (* -2 D (/ dx) (/ dx)))
  M)


;criticality search could be called like this:
#+test
(criticality-search #'gauss-seidel *D* *v-sigma-f* *sigma-a* *left-albedo* *right-albedo*)


