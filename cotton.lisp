;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name: Josephine Garces                           Date: 3/18/2014
;;; Course: ICS313        Project: Cotton
;;; File: cotton.lisp		Assignment: 6
;;;
;;; A horror-esque text-based adventure game using the
;;; functions and macros built from The Wizard's Game 
;;; in Conrad Barski's Land of Lisp. Slightly more interesting
;;; and convoluted! It is not that scary.

(in-package :User) ; optional 

(defconstant +ID+ "Josephine Garces")                                   ; ID is the author's name
(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* 't)

;; nodes are the areas which can be explored
(defparameter *nodes* '((room-4 (the room is a picture of decay with only a faded
				 number identifying it as room-4. the bed you were lying on
				 is stained with what looks like dried blood. could it be your blood?
				 no - it is not. the only way out of the room aside from the door
				 to the corridor is a window that is boarded shut. it looks like it
				 has been like that for decades.))
                        (corridor (the corridor is pitch black. it does not look like you will be
				 able to get further ahead without something to light your way.
				 you can try walking ahead if you want.))
			(dark (it is dark. you feel something grab you by the legs.
				 you try to wriggle out of its grasps but it overtakes you.
			 	 one of its limbs covers your mouth and you cease to breath. you are dead!))))

;; Describes the location
(defun describe-location (location nodes)
"Describes the location."
   (cadr (assoc location nodes)))

;; edges are the paths that can be take from a node
(defparameter *edges* '((room-4 (corridor west door))
				(corridor (room-4 east door))))

;; Describes a path from the current node
(defun describe-path (edge)
"Describes a path from the current node."
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; Describes all the paths from the given location
(defun describe-paths (location edges)
"Describes all the paths from the given location."
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(candle match))          
							         	; objects is the list of objects that exist in the world

(defparameter *object-locations* '((candle room-4)                      ; object-locations contain a list of objects and their location
                                   (match room-4)))

;; Lists the objects visible from a given location
(defun objects-at (loc objs obj-loc)
"Lists the objects visible from a given location."
   (labels ((is-at (obj)                                                ; Takes the symbol for an object
              (eq (cadr (assoc obj obj-loc)) loc)))                     ; Checks if the object is associated with the location
       (remove-if-not #'is-at objs)))                                   ; Removes objects not associated with the given location
       
;; Describes the objects visible at a given location
(defun describe-objects (loc objs obj-loc)
"Describes the objects visible at a given location."
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))                      
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'room-4)                                       ; location initializes the start of the game to the living room 
    
;; Describes everything at the current location
(defun look (&rest others)
"Describes everything at the current location."
  (append (describe-location *location* *nodes*)                        ; Returns a list of descriptions of location,
          (describe-paths *location* *edges*)                           ; paths, and
          (describe-objects *location* *objects* *object-locations*)))  ; objects

;; Takes a direction and moves the user to the associated location
(defun walk (&optional (direction "nowhere") &rest others)     
"Takes a direction and moves the user to the associated location"
(cond 
	((equal direction "nowhere")
		`(where are you going?))
	((not(equal others nil)) `(dizzy? you cannot walk like that.))	
	((eq direction 'ahead) 
		(defparameter *location* 'dark) 
		(pushnew 'die *allowed-commands*) 
		(die) (look)) 
(t
  (labels ((correct-way (edge)                                          ; Checks the edges to find the proper path
             (eq (cadr edge) direction)))                               ; based on the given direction
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next                                                          ; If the direction is valid,
          (progn (setf *location* (car next))                           ; the user moves to the associated location 
				   (look))				; Description of the new location is returned
          '(you cannot go that way.)))))))                              ; Otherwise, an error message


;; Moves the object from the current location to a new location: the user's "body"
(defun pickup (&optional (object "nothing") &rest others) 
"Moves the object from the current location to a new location: the user's body."                                                 
									; Checks if the object is a valid object in the current location
  (cond 
	  ((equal object "nothing") 
		  `(you have to specify what to pickup.))
		  ((not(equal others nil)) `(you cannot pickup more than one item.))
	  ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)                  ; Adds the object to the body location if it's a valid object
         `( - you are now carrying the ,object -))
	  (t '(you cannot get that.))))                                 ; Otherwise, an error message

;; Returns a list of objects the user has picked up
(defun inventory (&rest others)
"Returns a list of objects the user has picked up."
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; Returns the object if it exists in the user's inventory else NIL
(defun have (object) 
"Returns the object if it exists in the user's inventory else NIL."                                                    
    (member object (cdr (inventory))))
	
;; Provides a list of the available commands to the user 	
(defun help (&rest others)
	(game-print '(<--------------------HELP------------------------>))
	(game-print '(enter quit or one of the following commands - )) 
	(game-print `( ,@*allowed-commands*))
	(game-print '(<------------------------------------------------>)))

;; Help shortcut
(defun h (&rest others)
"Help shortcut."
	(help))

;; Help shortcut
(defun ? (&rest others)
"Help shortcut."
	(help))
	
;;;	 M A C R O S 

;; A macro to simplify adding new actions to the game.
(defmacro game-action (command subj obj place &body body)
	  `(progn (defun ,command (&optional (subject "nothing") object &rest others)
	            (if (and (eq *location* ',place)
	                     (eq subject ',subj)
	                     (eq object ',obj)
	                     (have ',subj)
						 (equal others nil))
	                ,@body
	            '(you cannot ,command like that.)))
	          (pushnew ',command *allowed-commands*)))	

;; A macro to simplify adding new objects into the world.		
(defmacro new-object (object location)
"new-object macro adds a new object to the world. 
Takes an object and location as a parameter."
`(cond
	((member ',object *objects*)
	'(that object already exists.))			; Check if the object already exists	    
	((not (assoc ',location *nodes*))
	'(that location does not exist.))		; Checks if the location exists
	((push ',object *objects*)
	(push '(,object ,location) *object-locations*)
	'(the new object was added.))))			; Otherwise add object

;; A macro to simplify adding new locations into the world.	
(defmacro new-location (location &body body)
"new-location macro adds a new location to the world."
`(cond
	((assoc ',location *nodes*)
	'(that location already exists.))		; Check if the location already exists
	((null ',body)
	'(description cannot be null.))			; Checks if the description is null
	((push '(,location (,@body)) *nodes*)
	 (push '(,location) *edges*)
	'(the new location was added.))))		; Otherwise adds location

;; Helper function for new-path macro.
(defun edge-eval (origin destination)
"edge-eval checks if the path from an origin to a destination exists."
(member destination (mapcar #'cadr (cdr (assoc origin *edges*)))))	

;; A macro to simplify adding new paths between locations in the world.		
(defmacro new-path (origin direction destination path)
"new-path macro adds a new path in the world. To make a two-way path, destination and origin arguments have to be swapped."
`(cond
	((not
		(or 
		(assoc ',origin *nodes*)
		(assoc ',destination *nodes*)))
		'(one must enter existing locations.))	; Checks if the orgin and destination are existing locations
		((edge-eval ',origin ',direction)
		'(that path exists.))			; Checks if a path exists between those two locations
		 ((nconc (assoc ',origin *edges*) (list (list ',destination ',direction ',path)))
		'(the new path was added.))))		; Otherwise adds the path


;; Ensures that the intro is printed only at the start of the game. 
(defparameter *intro-made* nil)

;; Custom REPL for the Wizard's World
(defun game ()
(cond ((equal *intro-made* nil) 
(princ "You open your eyes, and you are greeted by an unfamiliar ceiling.
Startled, you get to your feet and quickly scan your surroundings. It's
dark except for the stream of light coming from a crack on the only boarded
window in the room. You try to peek through the crack, but you cannot see
anything. You wonder where you are and who could have possibly brought you here.") 
(terpri)
(help)
(setf *intro-made* 't)))
    (let ((cmd (game-read)))						; Reads a command
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))				; Evaluates the command
            (game))))						; Prints the command
				
;; Custom read function to make user input simpler without having to type in parentheses				
(defun game-read ()
"Custom read function to make user input simpler without having to type in parentheses."
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; List of allowed commands with the addition of help h and ?
(defparameter *allowed-commands* '(look walk pickup inventory help h ?))

;; Given a command, this function checks if it is valid
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;; game-print helper function
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))		; Checks each character for various conditions
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))  ; Capitalizes or lowercases each character appropriately
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
			
;; Converts symbol-based writing into properly capitalized text using its helper function
(defun game-print (lst)
"Converts symbol-based writing into properly capitalized text."
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
	
			  
;; Initializing the candle to be not lit.	  
(defparameter *candle-lit* nil)

;; Use a match to light the candle in order to see what is in 
;; the players way. This actions triggers access to new rooms and
;; a whole set of new actions for the player to try.
(game-action light match candle room-4
	(if (and (have 'match) (have 'candle) (not *candle-lit*))
		(progn (setf *candle-lit* 't)
		(setf *objects* (remove 'candle *objects*))
		(setf *objects* (remove 'match *objects*))
	
		(new-object lit-candle room-4)
		(pickup 'lit-candle)
		
		(setf (second *nodes*) '(corridor (the corridor is lit with the candle.
			it is so long that you cannot see to the end.
			you notice that there are words written on the wall.)))
			
		(new-location end-of-corridor it feels eerie here. the air turns frigid.
			you feel as if someone is behind you. you turn around but there is no one there.
			the large looming door to the north can be opened with a key.)
		(new-path corridor north end-of-corridor way)
		(new-path end-of-corridor south corridor way)
		
		(new-location room-7 this room could easily be mistaken for a bathroom.
			it certainly smells like you just halved you life. it looks like it
			was once a laboratory. there are tubes and beakers of all shapes and
			sizes. the jars lining the walls seem to contain... human organs.
			there is a welding machine in the corner.)
			
		(pushnew 'strike *allowed-commands*)
		(new-object note1 room-7)
		(new-object scalpel room-7)
		(new-path room-7 north corridor door)
		(new-path corridor south room-7 door)
		
		(new-location basement-corridor you barely manage to breathe down here.
			each time you inhale fills your lungs with dust.
			you can open the door to the west with a key.)
		(pushnew 'unlock *allowed-commands*)
		(new-path room-7 downstairs basement-corridor stair)
		(new-path basement-corridor upstairs room-7 stair)
			
		(new-location room-9 you feel the urge to vomit. the sight of a corpse
			strapped to a hospital bed welcomes you. it is skeletal yet well preserved. it
			seems to have been strapped there for so long its skin has grown over the restraints.)
		(new-object note2 room-9)
		(new-object rod room-9)
		(new-object key room-9)
		(new-path room-9 west end-of-corridor door)
		(new-path end-of-corridor east room-9 door)
		
 		(new-location torture-room the room is unmarked but the word torture comes to mind.
			you want to step out but the various medieval-looking torture devices
			capture your attention. there is a strappado... a saw.. and an electric-chair.
			these look like they were used for more than just a display.)
		
		(new-path basement-corridor north torture-room door)
		(new-path torture-room south basement-corridor door)
		(new-object note3 torture-room)
		
		(pushnew 'study *allowed-commands*)
		'(the candle is now lit. it illuminates everything in the room.))
		'(you do not have a match.)))
		
;; Initializing the weapon to not been made.		
(defparameter *weapon-made* nil)

;; Action to weld the scalpel and rod together.
(game-action weld scalpel rod room-7
		(if (and (have 'scalpel) (have 'rod) (not *weapon-made*))
			(progn (setf *weapon-made* 't) (setf *objects* (remove 'scalpel *objects*))
			(setf *objects* (remove 'rod *objects*))
			(new-object spear room-7)
			(pickup 'spear)
			'(the scalpel is now securely welded to the rod.))
			'(you do not have anything you can weld together)))
		

;; Function to study things and find out more info.
(defun study (&optional (wall "nothing") &rest others)
"This function allows the survivor to study the walls, notes, and objects. One can use it to find out more information!"
	(cond
		((equal wall "nothing") 
		`(you need to specify a direct object.))
		((not(equal others nil)) `(you can only read one thing at a time.))
		((and
			(eq *location* 'corridor)
			(eq wall 'wall)
			(have 'lit-candle))
			'(the following words are scratched onto the wall - get out of here!
				be careful! do not die! do not go to the basement. please survive!))
		((and 
			(eq *location* 'end-of-corridor)
			(eq wall 'wall)
			(have 'lit-candle))
			'(the following words are scratched onto the wall - Help. You will never
				get out of here alive.))
		((and 
			(have 'note1) 
			(eq wall 'note1))
			'(Casefile 62478 - Shark Mouth. Her name is Kelsea. Her parents left her here with the best intentions.
				She first came here at the age of twelve with her parents hoping that her muteness would be treated and cured.
				As the years passed the daily visits dwindled. Once a year and then none.
				That did not help Kelsea at all. She secluded herself.
				I prescribed that the only way to cure the patient was to remove the offending infection.
				We started with her teeth but she only reacted negatively. We moved on to her tongue and that made her worse.
				At this point we have decided to stop all treatments because a growing hole has made itself apparent on her face.
				Oddly enough a tooth has grown back. - Doctor Cotton))
		((and 
			(have 'note2) 
			(eq wall 'note2))
			'(Casefile 62905 - The Monster. Everyone calls him Nicholas.
				Now he certainly was not jolly. He looked at everyone with disdain and he constantly muttered to himself.
				It was always something unintelligible. He was not deemed dangerous as most of the other patients who were admitted to this
				ward but we were short on subjects to test our treatments on. No one is ever willing but his protests became more violent.
				We tried to explain to him that it was for his own good but he only called us monsters. - Doctor Cotton))		
		((and 
			(have 'note3)
			(eq wall 'note3))
			'(Casefile 77777 - The Player. No one knew if this person had a name. No one even knows how this person got here.
				What everyone called this person was the player - because the sole focus of this person were games.
				A detachment from the real world and a lack of interest to maintain any human relationships were symptoms of the disease.
				On days we were able to communicate with the patient... the patient would try to explain the game world.
				The experience is like that of waking up in a strange room then being tasked to find a way to win without
				being given sufficient background information. Are you stuck in the game world? - Doctor Cotton))
		((and
			(have 'letter) 
			(eq wall 'letter))
			'(Dear Mom and Dad... when will you get me out of here? - Kelsea))
			
		((and 
			(eq wall 'strappado)
			(eq *location* 'torture-room))
			'(the strappado is a form of torture in which the hands of the victim are first tied behind his or her back
				and suspended in the air by means of a rope atached to the wrist which most likely dislocates
				both arms. Weights may be added to the body to intensify the effect and increase the pain.))
		((and 
			(eq wall 'saw)
			(eq *location* 'torture-room))
			'(the term death by sawing denotes the act of sawing a living person in half either longitudely or traversely
				through the central body mass. Usually a person is hung upside-down and sawn apart vertically through the middle
				starting at the groin.))
		((and 
			(eq wall 'electric-chair)
			(eq *location* 'torture-room))
			'(the electric-chair does as its name implies. the unfortunate subject whose life is sentenced to end by this method
				is electrified.))				
		(t '(there is nothing to study.))))
		

;; Initializes the state in which the girl has not been poked.
;; Helps manage flow of the game.		
(defparameter *poked-already* nil)

(defun poke (&optional (girl "nothing") &rest others)
"This function does what it says: poke!"
	(cond 
		((equal girl "nothing") 
		`(you need to specify a direct object.))
		((not(equal others nil)) `(you can only specify one thing to poke.))
		((and
			(eq *location* 'basement-room-2)
			(eq girl 'girl)
			(equal *shark-dead* nil)
			(equal *poked-already* nil))
			(setf *poked-already* 't)
			'(she slowly turns around. where her face should be is a gaping hole lined with shark-like teeth.
				perhaps you should not have poked her. she lunges at you and takes a bite of your arm.
				you can still run away but who knows how long you will survive. perhaps you can defend yourself.
				or leave now while you still can.))
		((and 
			(eq *location* 'basement-room-2)
			(eq girl 'girl)
			(equal *shark-dead* nil)
			(equal *poked-already* t))
			(pushnew 'die *allowed-commands*)
			(die)
			'(so you poked her again. this time she goes for your head. you have been decapitated. sorry you lose!))
		((and
			(eq *location* 'room-10)
			(eq girl 'monster))
			(pushnew 'die *allowed-commands*)
			(die)
			'(the monster screeches. it does not like being touched. it makes a grab for your weapon
				and it impales you with it. you are dead.))	
		(t' (there is nothing to poke.))))


;; Initial state of the girl - alive.
(defparameter *shark-dead* nil)

;; Initial state of the monster - alive.
(defparameter *boss-dead* nil)
	
;; Function to strike at an opponent. 	
(defun strike (&optional (enemy "nothing") &rest others)
"This function is your only defense against the monsters of this asylum."
(cond	
	((equal enemy "nothing") 
		`(you need to specify a direct object.))
	((not(equal others nil)) `(you can only specify one thing to strike.))
	((and
		(eq *location* 'basement-room-2)
		(equal *shark-dead* nil)
		(eq enemy 'girl)
		(have 'scalpel))
		(setf *shark-dead* 't)
		(new-object master-key basement-room-2)
		(new-object letter basement-room-2)
		'(you strike at the girl with your scalpel. you manage to cut her and render her immobile.
			although not yet dead... she loses interest in you and goes back to her original position.
			it seems like she will not be getting up any time soon. you notice she dropped some things.))			
	((and
		(eq *location* 'basement-room-2)
		(equal *shark-dead* nil)
		(eq enemy 'girl)
		(have 'spear))
		(setf *shark-dead* 't)
		(new-object master-key basement-room-2)
		(new-object letter basement-room-2)
		'(you strike at the girl with your spear. you manage to cut her and render her immobile.
			although not yet dead... she loses interest in you and goes back to her original position.
			it seems like she will not be getting up any time soon. you notice she dropped some things.))		
	((and
		(eq *location* 'room-10)
		(equal *boss-dead* nil)
		(eq enemy 'monster)
		(have 'scalpel))
		(pushnew 'die *allowed-commands*)
		(die)
		'(you strike at the monster with the scalpel. the handle is too short and you lose your grip.
			the scalpel flies across the room. the monster does not waste a chance and eats your head.
			you lose!))
	((and 
		(eq *location* 'room-10)
		(equal *boss-dead* nil)
		(eq enemy 'monster)
		(have 'spear))
		(new-object cake room-10)
		(pushnew 'win *allowed-commands*)
		(win)
		'(you strike at the monster. its long arms try to grab a hold of you but you strike again.
			you land more blows and manage to get the monster on his back.
			you take the chance to thrust the spear into its head. you win! the monster dropped something.))
		(t' (you cannot strike.))))


;; Initial state of the door in the basement-corridor.			
(defparameter *door-unlocked* nil)	

;; Initial state of the door at the end-of-corridor.
(defparameter *end-unlocked* nil)	
		
(defun unlock (&optional (door "nothing") &rest others)
"This function allows the user to unlock doors assuming he or she carries the required object."
	(cond
		((equal door "nothing") 
			`(you need to specify a direct object.))
		((not (equal others nil)) `(you can only specify one thing to unlock.))
		((and 
			(eq *location* 'basement-corridor)
			(equal *door-unlocked* nil)
			(eq door 'door)
			(have 'key))
			(pushnew 'poke *allowed-commands*)
			(setf *door-unlocked* 't)
			(new-location basement-room-2 it looks like this room was used for storage.
				there is nothing much inside aside from the various hospital equipment pushed against the walls.
				there is a figure before you. looking closer you observe that
			    it is a girl with long hair wearing a ragged hospital gown. she has her back turned towards you
				so you cannot see her face. you call out to her but she does not respond.)
			(new-path basement-room-2 east basement-corridor door)
			(new-path basement-corridor west basement-room-2 door)
			'(you have unlocked the door to room-2.))	
		((and
			(eq *location* 'end-of-corridor)
			(equal *end-unlocked* nil)
			(eq door 'door)
			(have 'master-key))
			(setf *end-unlocked* 't)
			(new-location room-10 what it is... you do not know. it rises. it is a mass of tangled limbs.
				the monster clicked its mandibles and it locks on you with its enormous compound eye.)
			(new-path room-10 south end-of-corridor door)
			(new-path end-of-corridor north room-10 door)
			'(you have unlocked the door to room-10))	
		((and
			(equal *door-unlocked* t) 
			(eq *location* 'basement-corridor)) 
			'(you have already unlocked the door))
		((and
			(equal *end-unlocked* t)
			(eq *location* 'end-of-corridor))
			'(you have already unlocked the door))	
			(t '(you cannot unlock the door.))))

			
;; An action to die.			
(defun die ()
"This action brings the player to the end of the game, where they cannot do anything.
Returns a message."
	(setf *allowed-commands* (remove 'study *allowed-commands*))
	(setf *allowed-commands* (remove 'poke *allowed-commands*))
	(setf *allowed-commands* (remove 'unlock *allowed-commands*))
	(setf *allowed-commands* (remove 'light *allowed-commands*))
	(setf *allowed-commands* (remove 'look *allowed-commands*))
	(setf *allowed-commands* (remove 'walk *allowed-commands*))
	(setf *allowed-commands* (remove 'pickup *allowed-commands*))
	(setf *allowed-commands* (remove 'weld *allowed-commands*))
	(setf *allowed-commands* (remove 'inventory *allowed-commands*))
	(setf *allowed-commands* (remove 'strike *allowed-commands*))
	'(you are dead. you cannot do anything. please start a new game!)
)

;; An action to win.
(defun win ()
"This action brings the player to the end of the game, where they cannot do anything.
Returns a congratulatory message."
	(setf *allowed-commands* (remove 'study *allowed-commands*))
	(setf *allowed-commands* (remove 'poke *allowed-commands*))
	(setf *allowed-commands* (remove 'unlock *allowed-commands*))
	(setf *allowed-commands* (remove 'light *allowed-commands*))
	(setf *allowed-commands* (remove 'look *allowed-commands*))
	(setf *allowed-commands* (remove 'walk *allowed-commands*))
	(setf *allowed-commands* (remove 'pickup *allowed-commands*))
	(setf *allowed-commands* (remove 'weld *allowed-commands*))
	(setf *allowed-commands* (remove 'inventory *allowed-commands*))
	(setf *allowed-commands* (remove 'strike *allowed-commands*))
	'(the monster dropped cake but you cannot eat that. can you? you have won. turn off your computer and go play outside.)
)