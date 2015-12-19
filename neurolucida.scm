
;;
;; Neurolucida XML file manipulation.
;;
;; Copyright 2011-2015 Ivan Raikov.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(require-extension files data-structures srfi-1 srfi-13  getopt-long sxml-transforms sxpath sxpath-lolevel ssax)
(require-extension typeclass kd-tree iset digraph format-graph  )

(import-instance (<KdTree> KdTree3d)
		 (<Point> Point3d))


(define lookup-def 
  (lambda (k lst . rest)
    (let-optionals rest ((default #f))
      (alist-ref k lst eq? default))))


(define (quotewrapped? str)
  (and (string? str) (string-prefix? "\"" str) (string-suffix? "\"" str) ))


(define (quotewrap str)
  (cond ((quotewrapped? str) str)
	((string-any char-whitespace? str)
	 (string-append "\"" str "\""))
	(else str)))


; Returns attr-list node for a given obj 
;   or #f if it is absent
(define (sxml:attr-alist obj)
  (if (and (not (null? (cdr obj)))
	    (pair? (cadr obj)) 
	    (eq? '@ (caadr obj)))
	 (cdr (cadr obj))
	 #f))

;; obtain  child named n of a node
(define (sxml:kidn name node)
  ((select-first-kid (lambda (x)  (eq? (car x) name))) node))

;; obtain all children of a node named n
(define (sxml:kidsn name node)
  ((select-kids (lambda (x) (eq? (car x) name))) node))

(define (mkdir dir)
  (system (sprintf "mkdir -p ~a" (quotewrap dir))))


(define *quiet* #f)


(define (d fstr . args)
  (let ([port (if *quiet* (current-error-port) (current-output-port))])
    (apply fprintf port fstr args)
    (flush-output port) ) )


(define opt-defaults
  `(
    (key             . "color")
    (format          . "swc")
    (forest-grid     . "d:10,p:5,n:1")
    ))


(define (defopt x)
  (lookup-def x opt-defaults))

(define (symbol-upcase str)
  (string->symbol (string-upcase str)))


(define opt-grammar
  `(
    (forest-grid
     "print grid coordinates covering the entire forest (resolution is given in number of points)"
     (value       (optional "P:N[,D:N,N:1]")
		  (transformer ,(compose (lambda (xs) 
					   (map (lambda (x) 
						  (let ((kv (string-split x ":")))
						    (cons (string->symbol (string-downcase (car kv)))
							  (string->number (cadr kv))))) xs ))
					 (lambda (x) (string-split x ","))))
		  (default  ,(defopt 'forest-grid))))

    (permute-coords
     "permute coordinates of node points in NG format according to the given list of 1-based indices"
     (value (required INDICES)))

    (data-dir
     "set download directory (default is a randomly generated name in /tmp)"
     (single-char #\d)
     (value (required DIR)))

    (key
     "extraction key"
     (single-char #\k)
     (value       (required "FIELD")
		  (default  ,(defopt 'key))))

    (format
     "output format (swc, pts, vcg, ng)"
     (single-char #\f)
     (value       (required "FORMAT")
		  (default  ,(defopt 'format))))

    (version
     "print the current version of neurolucida and exit")

    (help  "Print help"
	    (single-char #\h))
  
  ))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (neurolucida:usage)
  (print "Usage: " (car (argv)) " [options...] operands ")
  (newline)
  (print "Where operands are XML Neurolucida files")
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (width 35)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))


;; Process arguments and collate options and arguments into OPTIONS
;; alist, and operands (filenames) into OPERANDS.  You can handle
;; options as they are processed, or afterwards.

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define data-dir (make-parameter #f))

(define (get-data-dir)
  (or (opt 'data-dir)
      (or (data-dir)
	  (let ([dir (create-temporary-directory)])
	    (data-dir dir)
	    dir ) ) ))


(define (create-temporary-directory)
  (let ((dir (or (get-environment-variable "TMPDIR") 
		 (get-environment-variable "TEMP") 
		 (get-environment-variable "TMP") 
		 "/tmp")))
    (let loop ()
      (let* ((n (current-milliseconds))	; current milliseconds
	     (pn (make-pathname dir (string-append "neurolucida-" (number->string n 16)) "tmp")))
	(cond ((file-exists? pn) (loop))
	      (else (mkdir pn) pn))))))


(define neurolucida-xmlns "http://www.mbfbioscience.com/2007/neurolucida")


(define (parse-sxml fpath)
  (with-input-from-file fpath
    (lambda () (cons '*TOP* (ssax:xml->sxml (current-input-port) `((nl . ,neurolucida-xmlns)))))))


(define (extract-element sxml element-name key-attr)
  (let ((element-fullname (string->symbol (conc "nl:" (->string element-name)))))
    (let* ((elements ((sxpath `(// nl:mbf ,element-fullname ))  sxml))
	   (keys     ((sxpath `(// ,element-fullname @ ,key-attr)) `(*TOP* ,@elements))))
      (zip keys elements))))


(define (key=? a b)  (string=? (cadar a) (cadar b)))


(define (partition-by-key a k key=?)
  (partition (lambda (x) (equal? (car x) k)) a))


(define (get-node-origin x)
  (let ((x (if (equal? 'nl:point (car x)) x (sxml:kidn 'nl:point x))))
    (let ((attrs (sxml:attr-alist x)))
      (list (string->number (car (lookup-def 'x attrs)))
            (string->number (car (lookup-def 'y attrs)))
            (string->number (car (lookup-def 'z attrs))))
      )))


(define (get-node-radius x)
  (let ((x (if (equal? 'nl:point (car x)) x (sxml:kidn 'nl:point x))))
    (let ((attrs (sxml:attr-alist x)))
      (/ (string->number (car (lookup-def 'd attrs))) 2.))))


(define (make-tree-graph label x)

  (let* ((sxml           `(*TOP* ,@(cdr x)))
         (g              (make-digraph label #f))
         (node-info      (g 'node-info))
         (node-info-set! (g 'node-info-set!))
         (add-node!      (g 'add-node!))
         (add-edge!      (g 'add-edge!))
         )

    (add-node! 0 'soma)

    (let ((initial-tree ((sxpath `(nl:tree))  sxml)))

      (let recur ((tree      initial-tree)
		   (parent   0)
		   (node-id  1))

      (let ((points+spines     ((sxpath `((*or* nl:point nl:spine))) tree))
            (subtrees          (append ((sxpath `(nl:tree)) tree)
                                       ((sxpath `(nl:branch)) tree))))

        ;; insert points in the dependency graph
        (let ((node-id1
               (car
               (fold (lambda (n i.lp) 
                       (case (car n)
                         ((nl:point)
                          (let ((i (car i.lp))
                                (lp (cdr i.lp)))

                            (if (not (equal? lp n))
                                (begin
                                  (add-node! i n) (cons (+ 1 i) n))
                                i.lp)))
                         ((nl:spine)
                          (let ((i (car i.lp)) (lp (cdr i.lp)))
                            (node-info-set! (- i 1) (append (node-info (- i 1)) (list n))) 
                            i.lp))
                         (else (error 'make-tree-graph "unknown node type" n))))
                     (cons node-id  #f)
                     points+spines))))
          
          ;; insert edges
          (if (pair? points+spines)
              (let ((fin (- node-id1 1)))
                (add-edge! (list parent node-id #f))
                (let inner-recur ((i node-id))
                  (if (< i fin)
                      (let ((j (+ 1 i))) 
                        (add-edge! (list i j  #f))
                        (inner-recur j))
                      ))
                ))
          
          (if (pair? subtrees)
              (let ((branch-node (- node-id1 1)))
                (node-info-set! branch-node `(nl:branch ,(node-info branch-node)))
                (fold (lambda (subtree i) 
                        (recur subtree branch-node (+ i 1))) node-id1 subtrees))
              node-id1)
          ))
      ))
    g))



;;
;; NG (Neurolucida Graph) format
;;
;; ((Branch ... (Node ...) (Branch ...) ) ... )
;; 
;; Where ... is one or more attributes:
;;
;;   origin x y z 
;;   radius r
;;   length l
;;   spine-density-linear
;;   branch-order-Soma
;;   branch-order-Terminal
;;

(define (make-ng key g #!key (permute-coords #f))

  (define (sum lst) (fold + 0. lst))


  (define (pdist2 a b)
    (let ((diff2 (lambda (i) (let ((v (- (list-ref a i) (list-ref b i)))) (* v v)))))
      (sum (list-tabulate 3 diff2))))
  

  ;; DFS that increases depth only when branches are encountered
  (define (branch-dfs-fold g fn fb roots x y 
                           #!key
                           (edge-target cadr)
                           (next-edges (g 'out-edges)))
    

    (define (traverse visited n x y)
      (if (bit-vector-ref visited n)
          (values visited x y)
          (let ((visited (bit-vector-set! visited n #t))
                (x1 (fn n x y)))
            (traverse-edges visited (next-edges n) x1 y))
          ))

    
    (define (traverse-edges visited elst x y) =
      (if (null? elst)
          (values visited x y)
          (let ((n (edge-target (car elst)))
                (es (cdr elst)))
            (if (bit-vector-ref visited n)
                (traverse-edges visited es x y)
                (let ((visited (bit-vector-set! visited n #t))
                      (x (fn n x y)))
                  (let ((enext (next-edges n))
                        (out-edges ((g 'out-edges) n)))
                    (let ((y (if (> (length out-edges) 1)
                                 (fb n out-edges x y) y)))
                      (let-values (((visited x y)  
                                    (traverse-edges visited enext x y)))
                        (traverse-edges visited es x y)
                        ))
                    ))
                ))
          ))

    
    (define (traverse-roots visited ns x y)
      (if (null? ns) (values visited x y)
          (let-values (((visited x y) (traverse visited (car ns) x y)))
            (traverse-roots visited (cdr ns) x y))))

    
    (traverse-roots (make-bit-vector ((g 'capacity))) roots x y)
    )


    
  ;;
  ;; Compute node order from the soma and from the terminals
  ;;
  
  (define (compute-node-orders g)
      
    (let ((roots ((g 'roots)))
          (terminals  ((g 'terminals))))

      (let-values (
                   ((visited node-branch-order final-branch-depth) ;; branch order from soma
                    (branch-dfs-fold g 
                                     (lambda (n x y) (s32vector-set! x n y) x) 
                                     (lambda (n es x y) (+ 1 y))
                                     roots
                                     (make-s32vector ((g 'capacity)) -1) ;; branch order per node
                                     0 ;; branch order
                                     ))
                   ((visited node-inverse-branch-order final-inverse-branch-depth) ;; branch order from terminals
                    (branch-dfs-fold g 
                                     (lambda (n x y) (s32vector-set! x n y) x) 
                                     (lambda (n es x y) (+ 1 y))
                                     terminals
                                     (make-s32vector ((g 'capacity)) -1) ;; branch order per node
                                     0 ;; branch order
                                     edge-target: car
                                     next-edges: (g 'in-edges)
                                     ))
                   )

        (let (
              ;; branch order from soma
              (node-depth node-branch-order)
              ;; branch order from terminals
              (inverse-node-depth node-inverse-branch-order)
              )
        
          `((node-depth . ,node-depth)
            (inverse-node-depth . ,inverse-node-depth)
            ))
          )))

  (define (compute-node-tree roots out-edges node-info 
			     node-orders node-depths inverse-node-depths)

    (let recur ((node (car roots)) (ax '()))
	     
      (let* ((info   (node-info node))
	     (spines (sxml:kidsn 'nl:spine info))
	     )
	
	
	(if (equal? info 'soma)
	    
	    (fold recur '() (map cadr (out-edges node)))
	    
	    (case (car info)
	      
	      ((nl:point)
	       (let* ((oes (out-edges node))
		      (next-node (and (pair? oes) (cadr (car oes))))
		      (cylinder-origin (get-node-origin info))
		      (cylinder-radius (get-node-radius info))
		      (cylinder-length (and next-node
					    (let ((node1-origin (get-node-origin (node-info next-node))))
					      (sqrt (pdist2 cylinder-origin node1-origin))
					      ))
				       )
		      )
		 
		 (assert (<= (length oes) 1))
		 (if cylinder-length
		     (if (not (positive? cylinder-length))
			 (error 'neurolucida "zero cylinder length" info (node-info next-node))))
		 
		 (let ((ax1 (cons
			     (if next-node
				 `(Node (origin . ,(or (and permute-coords (permute-coords cylinder-origin) )
						       cylinder-origin))
					(radius . ,cylinder-radius)
					(length . ,cylinder-length)
					(branch-order-Soma . ,(s32vector-ref node-depths node))
					(branch-order-Terminal . ,(s32vector-ref inverse-node-depths node))
					,@(if (null? spines) '() `((spine-density-linear . ,(/ (length spines) cylinder-length))))
					)
				 `(Terminal (origin . ,(or (and permute-coords (permute-coords cylinder-origin) )
							   cylinder-origin))
					    (radius . ,cylinder-radius)
					    (branch-order-Soma . ,(s32vector-ref node-depths node))
					    (branch-order-Terminal . ,(s32vector-ref inverse-node-depths node))
					    ))
			     ax)))
		   
		   (if next-node (recur next-node ax1) (reverse ax1))
		   
		   )
		 ))
	      
	      ((nl:branch)
	       (let* ((branch-origin (get-node-origin info))
		      (branch-radius (get-node-radius info))
		      )
		 
		 (cons
		  `(Branch (origin . ,(or (and permute-coords (permute-coords branch-origin) )
					  branch-origin))
			   (radius . ,branch-radius)
			   (branch-order-Soma . ,(s32vector-ref node-depths node))
			   (branch-order-Terminal . ,(s32vector-ref inverse-node-depths node))
			   (children . ,(map (lambda (x) (recur x '())) (map cadr (out-edges node))))
			   )
		  ax)
		 ))
	      
	      (else (recur (cdr nodes) ax)))
	    
	    ))
      ))
  
  (let* ((roots ((g 'roots)))
         (out-edges (g 'out-edges))
         (node-info (g 'node-info))
         (node-orders (compute-node-orders g))
         (node-depths (lookup-def 'node-depth node-orders))
         (inverse-node-depths (lookup-def 'inverse-node-depth node-orders))
         )


    (let ((tree
	  `(((key . ,key)) . 
	    ,(compute-node-tree roots out-edges node-info
				node-orders node-depths inverse-node-depths))))

      tree
      )))
     
  



;;
;; SWC format
;;
;; n T x y z R P
;;
;; n is an integer label that identifies the current point and
;; increments by one from one line to the next.
;;
;; T is an integer representing the type of neuronal segment, such as
;; soma, axon, apical dendrite, etc. The standard accepted integer
;; values are given below.
;;
;;     * 0 = undefined
;;     * 1 = soma
;;     * 2 = axon
;;     * 3 = dendrite
;;     * 4 = apical dendrite
;;     * 5 = fork point
;;     * 6 = end point
;;     * 7 = custom
;;
;; x, y, z gives the cartesian coordinates of each node.
;;
;; R is the radius at that node.
;;
;; P indicates the parent (the integer label) of the current point or
;; -1 to indicate an origin (soma).


(define (print-swc x)
  (define get (compose car alist-ref))

  (define (swctype typestr)
    (if (not (string? typestr))
        (error "invalid tree type" typestr)
        (cond ((string-ci=? typestr "Dendrite") 3)
              ((string-ci=? typestr "Basal") 3)
              ((string-ci=? typestr "Apical") 4)
              ((string-ci=? typestr "Axon") 2)
              ((string-ci=? typestr "Soma") 1)
              (else 3))
        ))
  
  (let ((sxml `(*TOP* ,@(cdr x))))

    (let* (
           (offset (make-parameter 0))
           (soma ((sxpath `(nl:contour))  sxml))
	   (soma-points ((sxpath `(nl:point @))  soma))
	   (soma-npoints (length soma-points))
	   (soma-indices (list-tabulate soma-npoints (lambda (x) (+ 1 x))))
           )

       (for-each (lambda (p n)
		  (let ((data (cdr p)))
		    (let ((T    1) ;; soma
			  (x  (get 'x data))
			  (y  (get 'y data))
			  (z  (get 'z data))
			  (R  (number->string (/ (string->number (get 'd data)) 2)))
			  (P  (- n 1))
			  )
		    (printf "~A ~A ~A ~A ~A ~A ~A~%" n T x y z R (if (zero? P) -1 P)))))
		soma-points soma-indices)
       
       (offset soma-npoints)

       (let ((treetype (let ((typeattr ((sxpath `(nl:tree @ type)) sxml)))
                         (if (pair? typeattr)
                             (swctype (sxml:text (car typeattr)))
                             3))))

         (let recur ((tree ((sxpath `(nl:tree))  sxml))
                     (parent-index soma-npoints))
           
           (let (
                 (points     ((sxpath `(nl:point @))  tree))
                 (subtrees   (append ((sxpath `(nl:tree))  tree)
                                     ((sxpath `(nl:branch))  tree))))
             
             (let* ((npoints (length points))
                    (indices (let ((k (+ 1 (offset)))) (list-tabulate npoints (lambda (x) (+ k x)))))
                    (parents (cons parent-index indices)))
               
               (fold (lambda (pt i par n)
                       (let ((data (cdr pt)))
                         (let ((T    (if (fx> n 0) treetype (if (null? subtrees) 6 5))) ;; dendrite/axon, or end point/bifurcation point
                               (x  (get 'x data))
                               (y  (get 'y data))
                               (z  (get 'z data))
                               (R  (number->string (/ (string->number (get 'd data)) 2)))
                               (P  par)
                               )
                           (printf "~A ~A ~A ~A ~A ~A ~A~%" 
                                   i T x y z R P)
                           (fx- n 1)
                           )))
                     (fx- npoints 1)
                     points indices parents)
               
               (let ((parent-index1 (+ npoints (offset))))
                 
                 (offset (+ (offset) npoints)) 
                 
                 (if (pair? subtrees)
                     (for-each (lambda (subtree) 
                                 (recur subtree parent-index1) )
                               subtrees)))
               ))
           ))

       ))
  )


(define (print-pts x)
  (define get (compose car alist-ref))

  (let ((sxml `(*TOP* ,@(cdr x))))

    (let* ((soma ((sxpath `(nl:contour))  sxml))
	   (soma-points ((sxpath `(nl:point @))  soma))
	   (soma-npoints (length soma-points)))

       (for-each (lambda (p)
		  (let ((data (cdr p)))
		    (let ((x  (get 'x data))
			  (y  (get 'y data))
			  (z  (get 'z data))
			  (R  (/ (string->number (get 'd data)) 2))
			  )
		      (printf "~A ~A ~A~%" x y z))))
		soma-points)

       (let recur ((tree ((sxpath `(nl:tree))  sxml))  )

 	 (let ((points     ((sxpath `(nl:point @))  tree))
	       (subtrees   (append ((sxpath `(nl:tree))  tree)
				   ((sxpath `(nl:branch))  tree))))
	   
	   (let ((npoints (length points)))
	     
	     (for-each (lambda (pt)
			 (let ((data (cdr pt)))
			   (let ((x  (string->number (get 'x data)))
				 (y  (string->number (get 'y data)))
				 (z  (string->number (get 'z data)))
				 (r  (/ (string->number (get 'd data)) 2))
				 )
			     (printf "~A ~A ~A~%" x y z)
			     )))
		       points)
	     
	       (if (pair? subtrees)
		   (for-each (lambda (subtree) (recur subtree) )
			     subtrees))
	     )))

    )))


(define (print-grid data-map options)

  (define get (compose car alist-ref))

  (let ((nbndpts (alist-ref 'p options))
	(b-radius (alist-ref 'd options))
	(min-neighbors (alist-ref 'n options))
	(bb.all-points
	 (fold 
	  (lambda (x ax)

	    (let ((sxml `(*TOP* ,@(cdr x))))
	      
	      (let recur ((trees ((sxpath `(nl:tree))  sxml))
			  (exs (car ax))
			  (all-points (cdr ax)))
		
		(if (null? trees) (cons exs all-points)
		    
		    (let ((points     ((sxpath `(nl:point @))  (car trees)))
			  (subtrees   (append ((sxpath `(nl:tree))  (car trees))
					      ((sxpath `(nl:branch))  (car trees)))))
		      
		      (let (
			    (exs.all-points
			     (fold (lambda (pt ax)

				     (let ((data (cdr pt)))

				       (let ((x  (string->number (get 'x data)))
					     (y  (string->number (get 'y data)))
					     (z  (string->number (get 'z data))))
					 
					 (let* ((exs (car ax))
						(all-points (cdr ax))
						(x-min (list-ref exs 0))
						(x-max (list-ref exs 1))
						(y-min (list-ref exs 2))
						(y-max (list-ref exs 3))
						(z-min (list-ref exs 4))
						(z-max (list-ref exs 5)))
					   
					   (cons 
					    (list (min x x-min) (max x x-max)
						  (min y y-min) (max y y-max)
						  (min z z-min) (max z z-max))
					    (cons (make-point x y z) all-points))
					   ))
				       ))
				   (cons exs all-points) points))
			    )
			
			(let ((exs.all-points
			       (fold (lambda (subtree ax) (recur subtree (car ax) (cdr ax )))
				     exs.all-points subtrees)))
			  (recur (cdr trees) (car exs.all-points) (cdr exs.all-points)))
			))
		    ))
	      ))
	  (cons (list +inf.0 -inf.0
		      +inf.0 -inf.0
		      +inf.0 -inf.0) 
		'())
	  data-map))
	)
    
      (if (null? (cdr bb.all-points))
	  (error "empty point set in forest"))

      (let* (
	     (bb (car bb.all-points))
	     (x-min (list-ref bb 0))
	     (x-max (list-ref bb 1))
	     (y-min (list-ref bb 2))
	     (y-max (list-ref bb 3))
	     (z-min (list-ref bb 4))
	     (z-max (list-ref bb 5))
	     (point-tree (list->kd-tree (cdr bb.all-points)))
	     )
	
	(let* ((x-extent (- x-max x-min))
	       (y-extent (- y-max y-min))
	       (z-extent (- z-max z-min))
	       (x-step (ceiling (abs (/ x-extent nbndpts))))
	       (y-step (ceiling (abs (/ y-extent nbndpts))))
	       (z-step (ceiling (abs (/ z-extent nbndpts))))
	       )
	  
	  (printf "# x-min: ~A x-max: ~A y-min: ~A y-max ~A z-min: ~A z-max ~A~%" 
		  x-min x-max y-min y-max z-min z-max)
	  
	  
	  (let ( (bndpts 
		  (concatenate
		   (concatenate
		    (list-tabulate nbndpts 
			 (lambda (i) 
			   (list-tabulate nbndpts 
 			      (lambda (j) 
				(list-tabulate nbndpts 
			           (lambda (k) 
				     (make-point (+ x-min (* i x-step)) 
						 (+ y-min (* j y-step)) 
						 (+ z-min (* k z-step)) ))))))
			 ))
		   ))
		  )


	    (if (and b-radius min-neighbors)
		(for-each
		 (lambda (bp)
		   (let ((pts (kd-tree-near-neighbors point-tree b-radius bp)))
		     (if (and (not (null? pts))
			      (<= min-neighbors (length pts)))
			 (printf "~A ~A ~A~%" (coord 0 bp) (coord 1 bp) (coord 2 bp)))
		     ))
		 bndpts)
		(for-each
		 (lambda (bp)
		   (printf "~A ~A ~A~%" (coord 0 bp) (coord 1 bp) (coord 2 bp)))
		 bndpts)
		)
	    
	    )))
      ))



(define (main)

  (let ((operands          (opt '@)))

    (if (opt 'version) 
	(begin
	  (printf "neurolucida version ~?" "~A~%" 
		  (lookup-def 'version (extension-information 'neurolucida)))
	  (exit 0)
	  ))

    (if (null? operands) (neurolucida:usage))

    (d "data directory is ~s~%" (get-data-dir))

    (let* ((format (string->symbol (or (opt 'format) (defopt 'format))))
           (data-map
	    (concatenate
	     (map (lambda (p)
		    (let* ((key             (or (opt 'key) (defopt 'key)))
			   (contents        (parse-sxml p))
			   (contours        (extract-element contents "contour" key))
			   (trees           (extract-element contents "tree" key)))
		      (let recur ((keys (delete-duplicates (map car contours)))
				  (elms (append contours trees)) 
				  (data-map '()))

			(if (null? keys)
			    (if (null? elms) data-map (if (equal? format 'ng) data-map (error "elements with unknown color" elms)))
			    (let-values (((lst rest)  (partition-by-key elms (car keys) key=?)))
			      (recur (cdr keys) rest (cons (cons (car keys) (map cadr lst)) data-map)))))
			))
		  operands)))
	   (ddir (get-data-dir))
	   )

      (if (opt 'forest-grid)
	  (with-output-to-file (make-pathname ddir "forest.grid")
	    (lambda () (print-grid data-map (opt 'forest-grid))))

	  (case format
	    
	    ((pts)
	     (for-each (lambda (f x) 
			 (with-output-to-file f
			   (lambda () (print-pts x))))
		       (map (lambda (x) (make-pathname ddir (car (string-split (cadar x) "#")) ".pts")) data-map)
		       data-map))
	    
	    ((swc)
	     (for-each (lambda (f x) 
			 (with-output-to-file f
			   (lambda () (print-swc x))))
		       (map (lambda (x) (make-pathname ddir (car (string-split (cadar x) "#")) ".swc")) data-map)
		       data-map))
	    
	    ((vcg)
	     (for-each (lambda (f x) 
			 (with-output-to-file f
			   (lambda () 
			     ((make-format-graph 'vcg) (current-output-port) (make-tree-graph 'neurolucida x)))))
		       (map (lambda (x) (make-pathname ddir (car (string-split (cadar x) "#")) "vcg")) data-map)
		       data-map))
	    
	    ((ng)
	     (let* ((permute-coords (opt 'permute-coords))
		    (permute-coords 
		     (and permute-coords
			  (let ((permute-coords (map string->number (string-split permute-coords ","))))
			    (if (not (and (list? permute-coords)
					  (= (length permute-coords) 3)
					  (every (lambda (x) (and (integer? x) (<= 1 x) (>= 3 x))) permute-coords)))
				(error "invalid point coordinate permutation indices" permute-coords))
			    (let ((permute-coords (map (lambda (i) (- i 1)) permute-coords)))
			      (lambda (p) (map (lambda (i) (list-ref p i)) permute-coords))
			      ))
			  ))
		    )
	       
	       (for-each (lambda (f k x) 
			   (with-output-to-file f
			     (lambda () 
			       (pp (make-ng k (make-tree-graph 'neurolucida x) 
					    permute-coords: permute-coords)
				   (current-output-port)))))
			 (map (lambda (x) (make-pathname ddir (car (string-split (cadar x) "#")) "ng")) data-map)
			 (map (lambda (x) (car (string-split (cadar x) "#"))) data-map)
			 data-map)
	       ))
	  ))
      )))

(main)

