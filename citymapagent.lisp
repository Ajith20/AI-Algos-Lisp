(defun sample-test ()
; This is an example call to CityMapAgent
   (CityMapAgent '(118 131) "thirtyfirst" '(120 123) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM))

; States are represented as a 2-tuple giving the x and y coordinates  
; of the current point on the map.
;
; Nodes in the search tree will be represented by atoms 
;     A node for a point on the map will be represented by turning the 
;     string "Node-" followed by the x and y coordinates of the point 
;     into an atom.  
;     This can be done via the intern function.  Thus a node for the 
;     point (120,145) will be gotten via 
;     (intern(concatenate 'string "Node-" "120" "145"))
;     and will appear as |Node-120145| if printed.
; Nodes have the following properties:
;  state - The point on the map that is the current state of the node.  
;          For example, the node |Node-120145| will have as its state  
;          property the 2-tuple (120 145). This is the node's state, 
;          since it is the current location of the agent on the map.
;  road - a string giving the name of the road that the agent is
;         currently on at this state.
;  parent - The predecessor of the node on the best path that has
;           been found so far from start-point to the point 
;           represented by the node.
;  action - The action, such as (point1 point2 road) that was used to 
;           get from point1 to point2 represented by the node, where  
;           road is a string giving the name of the city street,   
;           avenue, or highway that was used to get to the given map 
;           point.
;  arc-cost - the cost of the arc for action used to get to the
;           state represented by the node
;  best-path-cost - The cost of the best known path from the initial 
;                   state to the node.
;  cost-to-goal-estimate - The estimate of the cost to a goal from 
;          the state represented by this  node
;  least-cost-estimate - The overall estimate of the cost from the
;           initial state to goal going through node


;
; CityMapAgent takes six problem-dependant arguments:
;
;     start-point - a point on the map that is the initial state 
;                   for the search, represented as a 2-tuple of
;                   x-coordinate and y-coordinate.
;     road - the name of the road that the agent is on at start-point,
;            represented as a string
;     goal-point - a point on the map that one wishes to reach,
;                  represented as a 2-tuple
;     goal-test? -  a predicate that returns true for goal nodes 
;                   and false for non-goal nodes.
;     get-successors -  a function to compute successors of a state
;                   represented by a node.  The successors are each
;                   represented as (new-state road arc-cost) triples.  
;     get-goal-estimate - a function which takes a point on the map  
;                         and returns an estimate of the distance 
;                         to a goal.point on the map.

; CityMapAgent returns a 2-tuple whose first element is an optimal 
; path from start-point to goal-point represented as a list of 
; actions that are performed to get from start-point to goal-point
; and whose second element is the cost of this path.
;

 (defun CityMapAgent
  (start-point road goal-point goal-test? get-successors 
   get-goal-estimate) 
;create a node for start-point, and find the path to goal-point 
;   using Algorithm A*"
  (defun search-graph (open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; search-graph is the function that iterates through the open list.
  ;     It selects the front node on open list and tests whether 
  ;     its state is the goal.  If so, it gets the best path that has 
  ;     been found to this node, along with the path cost and returns 
  ;     it.  Otherwise it recursively calls search-graph with the new 
  ;     open and closed lists that result from expanding the graph 
  ;     with the successors of the selected node.
  ; returns a 2-element list, containing the sequence of actions
  ;     leading to the goal and the total cost of the path;
  ;     adjacent actions are combined into a single action if
  ;     the adjacent actions are on the same road.
     (cond((null (car open-closed)) nil)
          (t (let((selected-node (caar open-closed)))
                 (terpri)
         
                 (format t 
                    "The nodes, f-values, and actions on open list are ~A" 
                     (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (car open-closed)))
                 (terpri)
                 (format t 
                     "The nodes, f-values, and actions on closed list are ~A" 
                      (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (cadr open-closed)))
                 (terpri) (terpri)
                 (format t "Select a new node from open list")
                 (terpri) 
                (format t "The selected node is ~A" 
                          (caar open-closed))
                (terpri)
                (format t "Check if this node is the goal node")
                (terpri)
                (cond((funcall goal-test? selected-node goal-point)
                          (terpri)
                          (format t "This is the goal node")
                          (terpri)
                          (format t "Here is the list of actions and total path cost in the solution")
                          (terpri)
                          (get-path-and-total-cost selected-node))
                     (t (let ((successors (funcall get-successors
                                                   selected-node)))
                        (format t "This is NOT the goal node")
                        (terpri)
                        (format t "Its successors (and their arc costs) are ~A"
                                  successors)
                        (terpri)

                        (search-graph
                           (expand-graph 
                             successors
                             selected-node
                             (list (cdr (car open-closed))
                                   (cons selected-node 
                                         (cadr open-closed)))
                             get-successors
                             get-goal-estimate 
                             goal-point)))))))))
                         
; create a node for start-city and begin the search
  (search-graph 
   (list(list (create-node start-point road 0 nil 0 nil 
                           get-goal-estimate goal-point))
   nil)))
      
 (defun expand-graph
   (succs parent-node open-closed succ-fn est-goal goal-point)
;(break "entering expand-graph")
        ;; succs is the list of sucessors of parent-node
        ;; each element of succs is a tuple of the form 
        ;;    (new-state road arc-cost) triples such as 
        ;;    ((100 122) "Kingshighway" 15).
	;; expand-graph adds the list of successors of parent to 
        ;;    the graph and to open list.
	;; It must make sure that a successor has not already 
        ;;    been encountered (ie., is not already on open 
        ;;    or closed) and must check for updating the 
        ;;    shortest path if the state has been encountered 
        ;;    before
        ;; returns the resulting 2-tuple giving the open 
        ;;    and closed lists

   (cond ((null succs) open-closed)
	 (t 
;         process the next successor
           (let* ((state (caar succs))
                   (point (caar succs))
                   (node-name 
                      (intern (concatenate 'string 
                                 "Node-" 
                                 (prin1-to-string (car point)) 
                                 (prin1-to-string (cadr point))
                                                  )))
		   (arccost (caddar succs))
                   (action (list (get parent-node 'state)
                                 (caar succs) 
                                 (cadar succs)))
		   (cost (+ (get parent-node 'best-path-cost)
			    arccost)))
              (format t "     The next successor is ~A" (car succs))
              (terpri)
              ;(break "in expand-graph")
              (cond ((and (not (state-on state (cadr open-closed)))
			  (not (state-on state (car open-closed))))
; this successor is not on open or closed list
                       (format t "this successor is not on open or closed list") 
                       (terpri)    
                       (expand-graph (cdr succs)
                                      parent-node
                                     (list (add-to-open 
                                           (create-node (caar succs) 
                                                     (cadar succs)
                                                     (caddar succs)
                                                     parent-node 
                                                     cost 
                                                     action 
                                                     est-goal 
                                                     goal-point)
                                            (car open-closed))
                                         (cadr open-closed))
                                      succ-fn
                                      est-goal
                                      goal-point))
		    ((and (state-on state (car open-closed))
                          (< cost (get node-name 'best-path-cost)))
; this successor is already on open list and we have
;    found a better path to it
                     (format t "**** ON OPEN AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                   (update-node-open node-name
                                                      parent-node
                                                      succ-fn
                                                      cost
                                                      action
                                                      open-closed)
                                    succ-fn
                                    est-goal
                                    goal-point))
                     ((and (state-on state (cadr open-closed))
                           (< cost (get node-name 'best-path-cost)))
; this successor is already on closed list and we have
;    found a better path to it
                     (format t "*** ON CLOSED AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                    (update-node-closed node-name
                                                        parent-node
                                                        succ-fn
                                                        cost
                                                        action
                                                        open-closed)
                                    succ-fn
                                    est-goal
                                    goal-point))
		    (t 
; this successor is already on open or closed and the new path
;   to the node is not better than the existing path
                      (format t "this successor is on open or closed but path is not better")
                      (terpri)
                      (expand-graph (cdr succs)
				    parent-node
				    open-closed 
				    succ-fn
				    est-goal
                                    goal-point)))))))

(defun update-node-open 
  (n parent successor-fn cost-of-short-path action open-closed )
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the open list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  

  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent. 
  ; successor-fn is the parameter giving the function for
  ;   computing successors 
  ; update the properties of node n and, if necessary, its position
  ;  on open list
  ; return the adjusted open-closed list
;(format t "entering update-node-open")
;(terpri)
(update-node n (car open-closed) cost-of-short-path parent action)
(append (list (rearrange-open (car open-closed) n (caar open-closed))) (list (cadr open-closed))))


(defun update-node(n open cost-of-short-path parent action)
;(format t "entering update-node")
;(terpri)
(and (setf (get n 'parent) parent) (setf (get n 'best-path-cost) cost-of-short-path) (setf (get n 'least-cost-estimate) (+ cost-of-short-path (get n 'cost-to-goal-estimate))) (setf (get n 'action) action)))




(defun rearrange-open(open n car-open)
;(format t "entering rearrange-open")
;(terpri)
(cond ((null open) ())
      ((and (equal (car open) car-open) (<= (get n 'least-cost-estimate) (get car-open 'least-cost-estimate)) (append (append (list n) (list (car open))) (rearrange-open (cdr open) n car-open))))
      ((and (>= (get n 'least-cost-estimate) (get (car open) 'least-cost-estimate)) (null (cdr open))) (append (append (list (car open)) (list n)) (rearrange-open (cdr open) n car-open)))
      ((and (>= (get n 'least-cost-estimate) (get (car open) 'least-cost-estimate)) (<= (get n 'least-cost-estimate) (get (cadr open) 'least-cost-estimate))) (append (append (list (car open)) (list n)) (rearrange-open (cdr open) n car-open)))
      ((equal n (car open)) (append '() (rearrange-open (cdr open) n car-open)))
      (t (append (list (car open)) (rearrange-open (cdr open) n car-open)))))




#|;(defun update-node-closed (n parent successor-fn cost-of-short-path 
                           action open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the closed list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  
  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent.  

  ; successor-fn is the parameter giving the function for
  ;   computing successors
  ; update the properties of node n and, if necessary, its
  ;   descendants on open and closed lists.
  ; return the adjusted open-closed list
;(and (update-node n (cadr open-closed) cost-of-short-path parent action) (append (list (update-open-nodes (df n successor-fn) (car ;open-closed) (distance n cost-of-short-path (cadr open-closed)))) (list (update-nodes-closed (df n successor-fn)) closed (distance n ;cost-of-short-path (cadr open-closed)))))))

;(defun convert-node(list)
(cond ((null list) ())
      (t (append (intern (concatenate 'string "Node-" (print1-to-string (caar list)) (print1-to-string (cadr (car list))))) (convert-node (cdr list))))))



(defun distance (n cost-of-short-path closed)
(cond ((null closed) ())
      ((equal n (car closed)) (- (get n 'cost-of-short-path) cost-of-short-path))
      (t (distance (cdr closed)))))

(defun df (n sucessor-fn)
(dfs (list n) sucessor-fn))


(defun dfs (list sucessor-fn)
(cond ((null list) ())
      ((or (node-checking (car list) (car open-closed)) (node-checking (car list) (cadr open-closed)))) (cons (car list) (dfs (cdr list) sucessor-fn)))
      (t (dfs (append (convert-node (funcall sucessor-fn (car list))) (cdr list)) sucessor-fn))))

(defun get-open-value (open)
(get (car (reverse open)) 'best-path-cost))

(defun update-open-nodes (list open distance)
(cond ((null (cdr list)) (update-open-nodes (cdr list) (rearrange-open open (car list) (car list)) distance))
      ((node-checking (car list) open) (and (setf (get (car list) 'best-path-cost) (- (get (car list) 'best-path-cost) distance)) (setf (get (car list) 'least-cost-estimate) (- (get (car list) 'least-cost-estimate) distance)) (update-open-nodes (cdr list) (rearrange-open open (car list) (car list)))))
      (t (update-open-nodes (cdr list) open distance))))

(defun node-checking (n list)
(cond ((null list) ())
      ((equal n (car list)) t)
      (t (node-checking n (cdr list)))))

(defun update-nodes-closed (list closed distance)
(cond ((null closed) ())
      ((node-checking (car closed) list) (and (setf (get (car closed) 'best-cost-path) (- (get (car closed) 'best-cost-path) distance)) (setf (get (car closed) 'least-cost-estimate) (- (get (car closed) 'least-cost-estimate)) distance) (append (list (car closed)) (update-nodes-closed list (cdr closed) distance))))
      (t (append (list (car closed)) (update-nodes-closed list (cdr closed) distance)))))
|#
(defun state-on (state lst)
;(break "entering state-on")
; state is a state represented as a 2-tuple giving the
;   coordinates of the point represented by the state
; lst is an open or closed list
; return true if a node on lst has this point as its state
(cond ((null lst) ())
     ((and (= (car state) (car (get (car lst) 'state))) (= (cadr state) (cadr (get (car lst) 'state)))) t)
     (t (state-on state (cdr lst))))
)
       
(defun add-to-open (n open)
; n is a node and open is the open list
; add n to the open list in the correct position 
; return the new open list
; YOU MUST WRITE THIS FUNCTION
;(format t "entering add-to-open")
;(terpri)
(cond ((null open) (list n))
      (t (add-open (car open) n open)))
)

(defun add-open (car-open n open)
;(format t "entering add-open")
;(terpri)
(cond ((null open) ())
      ((<= (get n 'least-cost-estimate) (get car-open 'least-cost-estimate)) (append (list n) open))
      ((and (>= (get n 'least-cost-estimate) (get (car open) 'least-cost-estimate)) (null (cdr open))) (append (append (list (car open)) (list n)) (add-open car-open n (cdr open))))
      ((and (>= (get n 'least-cost-estimate) (get (car open) 'least-cost-estimate)) (<= (get n 'least-cost-estimate) (get (cadr open) 'least-cost-estimate))) (append (append (list (car open)) (list n)) (add-open car-open n (cdr open))))
      (t (append (list (car open)) (add-open car-open n (cdr open))))))

(defun create-node 
  (point road arc-cost parent cost-of-short-path action est-goal goal-point)
  ; point is a 2-tuple representing a point on the map
  ; create a new node with this point as its state and
  ;   with the appropriate properties
  ; road is a string giving the name of the road that the
  ;   agent is on at point
  ; parent is the parent node.
  ; action is the action that moved from parent to point.  
  ; cost-of-short-path is the cost of the path from the
  ;   initial state to the state represented by this new
  ;   node and goes through parent.
  ; goal-point is a 2-tuple representing the goal
  ; est-goal is a parameter giving the function for estimating
  ;   the cost of getting to the goal from this new node 
  ; create a new node with the appropriate properties
  ; return the created node.
(let ((node (intern (concatenate 'string 
                                 "Node-" 
                                 (prin1-to-string (car point))
                                 (prin1-to-string (cadr point))
                                 ))))
  (setf (get node 'state) point)
  (setf (get node 'road) road)
  (setf (get node 'arc-cost) arc-cost)
  (setf (get node  'parent) parent)
  (setf (get node 'action) action)
  (setf (get  node 'best-path-cost) cost-of-short-path)
  (setf (get node 'cost-to-goal-estimate) (funcall est-goal point goal-point)) 
  (setf (get  node 'least-cost-estimate)
        (+ cost-of-short-path (get node 'cost-to-goal-estimate)))
  node))

    
(defun get-path-and-total-cost (node)
; node is a node in the graph
; return a list consisting of two elements: the path (in terms of 
;    successive actions) that was taken to get to node and and 
;   cost of that path
(append (condense-actions (get-path node)) (list (get node 'best-path-cost)))
)



(defun condense-actions(list)
;(format t "entering condense-actions")
;(terpri)
(cond ((null list) ())
      ((and (string= (caddr (car list)) (caddr (cadr list))) (and (= (car (cadr (car list))) (car (car (cadr list)))) (= (cadr (cadr (car list))) (cadr (car (cadr list)))))) (append (list (car (condense (car list) (cdr list)))) (condense-actions (cadr (condense (car list) (cdr list))))))
      (t (append (list (car list)) (condense-actions (cdr list))))))



(defun condense(car-list list)
;(format t "entering condense")
;(terpri)
(cond ((null list) ())
      ((and (string= (caddr (car list)) (caddr (cadr list))) (and (= (car (cadr (car list))) (car (car (cadr list)))) (= (cadr (cadr(car list))) (cadr (car (cadr list)))))) (condense car-list (cdr list)))
      (t (append (list (append (list (car car-list)) (list (cadr (car list))) (list (caddr car-list)))) (list (cdr list))))))


(defun get-path(node)
(cond ((null (get node 'parent)) (get node 'action))
      (t (append (get-path (get node 'parent)) (list (get node 'action))))))

(defun successors-CM (node)
(format t "entering sucessors-CM")
(terpri)
; node is a node in the search graph
; return a list of the successors of the state represented by
;   this node, with each successor given as
;   (new-point road arc-cost ) triples, such as 
;   ((100 122) "Kingshighway" 15)
      (cond ((and (not (equal (get (state-atom (get node 'state)) 'type) 'intersection)) (equal (get (intern (get node 'road)) 'type) 'city-street)) (city-street node (get node 'road)))
      ((and (not (equal (get (state-atom (get node 'state)) 'type) 'intersection)) (equal (get (intern (get node 'road)) 'type) 'avenue)) (avenue node (get node 'road)))
      ((and (not (equal (get (state-atom (get node 'state)) 'type) 'intersection)) (equal (get (intern (get node 'road)) 'type) 'highway)) (highway node (get node 'road)))
      ((equal (get (state-atom (get node 'state)) 'type) 'intersection) (intersection node (get (state-atom (get node 'state)) 'options)))))


(defun city-street(node road)
;(format t "entering city-street")
;(terpri)
(cond ((= (car (get node 'state)) (get (intern road) 'x-coord-start)) (list (append (list (append (list (+ 1 (car (get node 'state)))) (list (cadr (get node 'state))))) (list road) (list 2))))
      ((= (car (get node 'state)) (get (intern road) 'x-coord-end)) (list (append (list (append (list (- (car (get node 'state)) 1)) (list (cadr (get node 'state))))) (list road) (list 2))))
      (t (append (list (append (list (append (list (- (car (get node 'state)) 1)) (list (cadr (get node 'state))))) (list road) (list 2))) (list (append (list (append (list (+ 1 (car (get node 'state)))) (list (cadr (get node 'state))))) (list road) (list 2)))))))


(defun avenue(node road)
;(format t "entering avenue")
;(terpri)
(cond ((= (cadr (get node 'state)) (get (intern road) 'y-coord-start)) (list (append (list (append (list (car (get node 'state))) (list (+ 1 (cadr (get node 'state)))))) (list road) (list 2))))
      ((= (cadr (get node 'state)) (get (intern road) 'y-coord-end)) (list (append (list (append (list (car (get node 'state))) (list (- (cadr (get node 'state)) 1)))) (list road) (list 2))))
      (t (append (list (append (list (append (list (car (get node 'state))) (list (- (cadr (get node 'state)) 1)))) (list road) (list 2))) (list (append (list (append (list (car (get node 'state))) (list (+ 1 (cadr (get node 'state)))))) (list road) (list 2)))))))


(defun highway(node road)
(highway-succ (get (intern road) 'access-points) (car (get (intern road) 'access-points)) node road))



(defun highway-succ(list car-list node road)
;(format t "entering highway-succ")
;(terpri)
(cond ((and (= (car (get node 'state)) (car car-list)) (= (cadr (get node 'state)) (cadr car-list))) (list (append (list (cadr list)) (list road) (list (* 1.5 (sqrt (+ (* (- (caadr list) (car car-list)) (- (caadr list) (car car-list))) (* (- (cadr (cadr list)) (cadr car-list)) (- (cadr (cadr list)) (cadr car-list))))))))))
      ((and (= (car (get node 'state)) (caar (reverse list))) (= (cadr (get node 'state)) (cadr (car (reverse list))))) (list (append (list (cadr (reverse list))) (list road) (list (* 1.5 (sqrt (+ (* (- (caar (reverse list)) (caadr (reverse list))) (- (caar (reverse list)) (caadr (reverse list)))) (* (- (cadr (car (reverse list))) (cadr (cadr (reverse list)))) (- (cadr (car (reverse list))) (cadr (cadr (reverse list))))))))))))
      (t (highway_suc (car list) list node road))))


(defun highway_suc(car-list list node road)
;(format t "entering highway_suc")
;(terpri)
(cond ((null list) ())
      ((and (= (car (get node 'state)) (caar list)) (= (cadr (get node 'state)) (cadr (car list)))) (append (list (append (list car-list) (list road) (list (* 1.5 (sqrt (+ (* (- (car (get node 'state)) (car car-list)) (- (car (get node 'state)) (car car-list))) (* (- (cadr (get node 'state)) (cadr car-list)) (- (cadr (get node 'state)) (cadr car-list))))))))) (list (append (list (cadr list)) (list road) (list (* 1.5 (sqrt (+ (* (- (car (cadr list)) (car (get node 'state))) (- (car (cadr list)) (car (get node 'state)))) (* (- (cadr (cadr list)) (cadr (get node 'state)) (- (cadr (cadr list)) (cadr (get node 'state)))))))))))))
      (t (highway_suc (car list) (cdr list) node road))))

(defun state-atom(list)
(cond ((null list))
(t (intern (concatenate 'string (prin1-to-string (car list)) (prin1-to-string (cadr list)))))))

(defun intersection (node list)
;(format t "entering intersection")
;(terpri)
(cond ((null list) ())
      ((equal (get (intern (car list)) 'type) 'avenue) (append (avenue node (car list)) (intersection node (cdr list))))
      ((equal (get (intern (car list)) 'type) 'city-street) (append (city-street node (car list)) (intersection node (cdr list))))
      ((equal (get (intern (car list)) 'type) 'highway) (append (highway node (car list)) (intersection node (cdr list))))))

(defun replace-road (list string)
;(format t "entering replace-road")
;(terpri)
(cond ((null list) ())
     (t (append (append (list (caar list)) (list  string) (list (caddr (car list))) (replace-road (cdr list) string))))))


(defun goal-test-CM? (node goal-point)
; node is a node and goal-point is a 2-tuple giving the coordinates
;    of the goal point on the map
; return true if the state for this node is goal-point
(cond ((and (= (car goal-point) (car (get node 'state))) (= (cadr goal-point) (cadr (get node 'state)))) t)
      (t ()))
)

(defun get-goal-estimate-CM (point goal-point)
; point and goal-point are both 2-tuples representing points 
;   on the map 
; return an estimate of the cost of getting from point to goal-point
  (sqrt (+ (* (- (car goal-point) (car point)) (- (car goal-point) (car point))) (* (- (cadr goal-point) (cadr point)) (- (cadr goal-point) (cadr point)))))
)
