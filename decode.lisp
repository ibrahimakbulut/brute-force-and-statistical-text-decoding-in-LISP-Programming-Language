; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************

;Ibrahim Akbulut
;151044077

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"




(defun split-string (String_val)

(let ((strings '()) (string_lenght (length String_val) ) (a '() ) )


	(do ( (x 0 (+ x 1))  )


		( (= x string_lenght) x)

		(if (not (char= #\Space  (char String_val x)))
			(progn
				;(print (char String_val x))
			(push (char String_val x) a)

			)

			(progn
				(let ((string_size (list-length a)) (reversed_list '() ))

					(do ( (y 0 (+ y 1))   )	;words added to list seperator space
						( (= y string_size) y)

						(push (nth y a) reversed_list)
					)

				;(print a)
				(push reversed_list strings) ;as reversed,
				(setf a '())
				)
			)
		)
	)
	(let ((string_size (list-length a)) (reversed_list '() ))

		(do ( (y 0 (+ y 1))   )    ; last word of document added to list because 
			( (= y string_size) y)

			(push (nth y a) reversed_list)
		)

	;(print a)
		(push reversed_list strings)
		(setf a '())
	)


	(let ((strings_size (list-length strings)) (reversed_list '() ))

		(do ( (y 0 (+ y 1))   )  ;all words are reversed 
			( (= y strings_size) y)

			(push (nth y strings) reversed_list)
		)

		reversed_list
	)
	)
)

(defun read-as-list (filename)    ;;read decoded text file by assume one or more words can occur in a line and empty line must be occur after every paragrahp.
									;words are in reversed order in paragrahp so paragraphs must be reversed when use them
									; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
    (let (x '() paragraph '() all_list '() reversed_all_list'() )
	(with-open-file (stream filename)
    (progn
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line) x)
      
      (if (not (= (length line) 0) )
      (push  line paragraph)

      (progn
      	(push paragraph x)
      	(setf paragraph '())

      )
     )
     )
    (push paragraph x)
    )
    )
	x

	(do  ( (a 0 (+ a 1) ) )

		(  (= a (list-length x)) a)

		(let  (   (sublist '() )                )

			(do (  (k (- (list-length (nth a x)) 1) (- k 1) )          )
				( (< k 0 ) k )

				(progn
					(do (  (h 0 (+ h 1)  )                     )
						( (= h (list-length (split-string (nth k (nth a x) ) )) ) h )
					(push  (nth h (split-string (nth k (nth a x) ) ) )   sublist)

					)
				) 
			)
			(push sublist all_list)
		)     
    )
    all_list

    (do  (   (p (- (list-length all_list) 1)  (- p 1) )         )

    	( (< p 0)  p)

    	(progn
    		(push (reverse (nth p all_list) ) reversed_all_list)
    	)

    )
    reversed_all_list
    )
    )

(defun read-as-list3 (filename)     
	
    (let (x '())
	(with-open-file (stream filename)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line) x)
      
      (push  line x)
     
     )
    )

	x

	(let ( (y '()) (total_rows_size (list-length x)) )

		(do ( (z 0 (+ z 1)) )

			((= z total_rows_size) z)
			(let ( (list_of_row (split-string (nth z x))) ) 

			(do (  (h (- (list-length (split-string (nth z x) )) 1)  (- h 1) ))

				((< h 0) h)

				 (push (nth h list_of_row) y)
			)
 			)
			)
			y
		)
    )

    )
    

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***
(defun encoder (text decoded_text)  ; encoder function is for encode paragrahps that lists have two dimensions 

	(if (null (car text))

				(let ((strings_size (list-length decoded_text)) (reversed_list '() ))

					(do ( (y 0 (+ y 1))   )  ;all words are reversed 
					( (= y strings_size) y)

					(push (nth y decoded_text) reversed_list)
				)
				reversed_list
				)
			
		(let (  (temp_decoded_word '())  )
			(do ( (x (- (list-length (car text)) 1) (- x 1) ) )

				( (< x 0) )

				(push (i2c (mod (+ (c2i (nth x (car text))) 5) 26) ) temp_decoded_word)				
				)
			(push temp_decoded_word decoded_text)
			(encoder (cdr text) decoded_text)

			)
	)
	)
(defun spell-checker-0 (word dictionary)   ;this function checks whether word is in dictionary or not.
								;this function check th dictionary2.txt file by default if you want to 
								;  check your word in other dictionary file then change the stream parameter.

	(let ( (string_word (format nil "~{~A~}" word) ) (boolean nil) )

		    (let (x '())
			(with-open-file (stream dictionary) ;this is for reading dictionary files that assume every line has one word and all file has no empty lines between words.
   			 (do ((line (read-line stream nil)
               			(read-line stream nil)))
        				((null line) x)
      
      					(if (string-equal line string_word)

      						(setf boolean t)
      					)
      			)
   			 )
			)
		    boolean
	)
)

(defun spell-checker-1 (word words_list_as_hashmap) ;this function checks the word in a given hashmap.

	(let ( (boolean nil) )

		(if  (gethash (format nil "~{~A~}" word) words_list_as_hashmap)  

			(setf boolean t)


		)
		boolean
	)
)


;; -----------------------------------------------------
;; DECODE FUNCTIONS
(defun fill_all (target_list source_list)  ;helper function that fill elements of a list to another list.

(do (   (x (- (list-length source_list) 1) (- x 1))     )
	(    (< x 0) x                                )

	(progn
		(push (nth x source_list) target_list)

	)
)
target_list
)

(defun produce_all (number_variation_letters used_letters)  ;this function produce all different letter mappings according to number of 
															; letter variations and used letters in mapping.

	(if (equal number_variation_letters (list-length used_letters))

		(progn
			(let (  (a '() ) )

				(push used_letters a)
				a
			)
		)
	(let (   (sublist '()) (copy_used_letters used_letters)        )
	(do (        (x 0 (+ x 1) )          )
		( (> x 25) x                     )

		(progn

			(if (not (member (i2c x) used_letters))
				(progn
				;(print (i2c x))
				(push (i2c x) copy_used_letters)
				(setf sublist (fill_all sublist (produce_all number_variation_letters copy_used_letters)) )
				(setf copy_used_letters used_letters)

				)
			)
		)
	)
	sublist
	)
)
)


(defun change_words_letter_2 (new_letters_list will_change_list words_list)  ;this function is used to change letters with new letters
	(let ( (x (list-length words_list) ) )

		(do (  (y  (- x 1)  (- y 1) ) ) 

			( (< y 0) y)

			(let ( (temp_word (nth y words_list))    )
			(do (  (a (- (list-length temp_word ) 1)  (- a 1) ) )

				( (< a 0) a)

				(do (    (k 0 (+ k 1) ) )

				( (>= k (list-length new_letters_list)) k )

				(if  (char= (nth a temp_word) (nth k will_change_list) ) 
					(progn
					(setf (nth a (nth y words_list)) (nth k new_letters_list))
					(setf k 26)
					)
				)
				)
			)
			)
		)
	)
	words_list
	)
	
(defun list_all_letters_in_text (paragraph) ;this function is used to determine which letters are used in paragraph
	(let ( (used_letters '() ) (counter 0)       )

		(do (   (x (- (list-length paragraph) 1) (- x 1) )    )

			(  (< x 0) x  )

			(do (   (a 0 (+ a 1) )  )
				(  (= a (list-length (nth x paragraph))) a    )

				(if (not (member (nth a (nth x paragraph)) used_letters) )

					(progn
						(push (nth a (nth x paragraph)) used_letters)
						(setf counter (+ counter 1))
					)
				)
			)
		)
		used_letters
	)
)

(defun most_six_letters (paragraph)   ;this function is used to determine which most 6 letters are used in paragraph

	(let (   (used_letters '() ) (counter_of_letter '() )   (combine_both_used_and_counter_of_letters '() )   )
		(progn
		(do (   (x (- (list-length paragraph) 1) (- x 1) )    )

			(  (< x 0) x  )

			(do (   (a 0 (+ a 1) )  )
				(  (= a (list-length (nth x paragraph))) a    )

				(if (not (member (nth a (nth x paragraph)) used_letters) )

					(progn
						(push (nth a (nth x paragraph)) used_letters)
						(push 1 counter_of_letter)
					)
					(progn
					(setf (nth	(position (nth a (nth x paragraph)) used_letters) counter_of_letter) (+ (nth	(position (nth a (nth x paragraph)) used_letters) counter_of_letter) 1)  )

					)
				)
			)
		)

		(push counter_of_letter combine_both_used_and_counter_of_letters)
		(push used_letters combine_both_used_and_counter_of_letters)
		;(print combine_both_used_and_counter_of_letters)
		(let ( (most_six '() ) (maximum 0) (index 0) (looper 0) )
		(progn
		(if (>= (list-length used_letters) 6)
				(setf looper 6)
				(setf looper (list-length used_letters)) 
		)
		(do (   (c 0 (+ c 1) )                 )
			(   (= c looper)  c                     )
			(progn
			(do (   (k 0  (+ k 1) )                           )
				(   (= k (list-length counter_of_letter))     )
				(if (and (>= (nth k counter_of_letter) maximum) (not (member (nth k used_letters) most_six) )   )
					(progn
					(setf maximum (nth k counter_of_letter))
					(setf index k)
					)

				)
			)
			(push (nth index used_letters) most_six)
			(setf maximum 0)
			)
		)
		most_six
		)
		)
		
		)
	)
)

(defun just_make_copy_list (paragraph) ;this function is used to copy a two dimesional list like paragraph

	(let (   (x (list-length paragraph))  (empty_list '())       )

		(do (  (a (- x 1) (- a 1) )          )

			(  (< a 0)     a                  )

			(let ( (temp_word '()) (word_length (list-length (nth a paragraph)))   )

			(do (   (b (- word_length 1) (- b 1) )    )

				((< b 0)   b)

				(push (nth b (nth a paragraph)) temp_word)


			)

			(push temp_word empty_list)

			)

		)
		empty_list
	)

)
(defun Gen-Decoder-A (paragraph dictionary)

	(let ( (n (list_all_letters_in_text paragraph)) (my_hash (make_own_hash_table dictionary))  (saved paragraph) )         
		(let (  (all_possibilities	(produce_all (list-length n) '()) )  (counter 0) (record_number_correct_words 0) (decoded_text '(a b c))  )
			;(print n)
			;(print (nth 0 all_possibilities))
			;(print all_possibilities)
			(do  ( (a 0 (+ a 1))      )
				(  (= a  (list-length all_possibilities)  )  a )

						(progn
						(setf saved  (change_words_letter_2 (nth a all_possibilities) n (just_make_copy_list paragraph) ) ) 

						(do   (     (k 0 (+ k 1) )              )

						(  (> k (- (list-length saved) 1) ) k  )
						(progn

						(if (spell-checker-1 (nth k saved) my_hash)

							(progn
								;(print (nth k saved))
							(setf counter (+ counter 1))

							)

						)
						)

					)
					(if (> counter record_number_correct_words)
						(progn 
						(setf decoded_text saved)
						(setf record_number_correct_words counter)
						
						)
					)
					
					(setf counter 0)
					)

		)
			decoded_text

		)

		)
)
																													
(defun Gen-Decoder-B-0 (paragraph dictionary)

	(let (  (x (list_all_letters_in_text paragraph))  (y  (most_six_letters paragraph)  ) ( differant_letters '() ) (my_hash_B (make_own_hash_table dictionary))  (decoded_text '(a b c) )    )

		(progn 
		(do ( (k 0 (+ k 1) )   )
			( (= k (list-length x) )  k )

			(if (not (member (nth k x) y ) )
				(push (nth k x) differant_letters)

			)
		)
		;(print differant_letters)
		;(print y)

		;(print (list-length differant_letters))
		(let (    (all_possibilities (produce_all (list-length x) y ))  (saved paragraph) (counter_1 0) (record_number_correct_words_1 0) )

			;(print all_possibilities)

			(do  ( (a 0 (+ a 1))      )
				(  (= a  (list-length all_possibilities)  )  a )

						(progn
						(setf saved  (change_words_letter_2 (append (subseq '(#\n #\i #\o #\a #\t #\e) (-  (- 6 (- (list-length x) (list-length differant_letters) ) ) 0) 6) (subseq (nth a all_possibilities) 0  (list-length differant_letters)  )) (append y differant_letters) (just_make_copy_list paragraph) ) ) 
						;(print saved)

						(do   (     (z 0 (+ z 1) )              )

						(  (> z (- (list-length saved) 1) ) z  )
						(progn

						(if (spell-checker-1 (nth z saved) my_hash_B)

							(progn
							(setf counter_1 (+ counter_1 1))

							)


						)
						)

					)
					(if (> counter_1 record_number_correct_words_1)
						(progn 
						(setf decoded_text saved)
						(setf record_number_correct_words_1 counter_1)
						
						)

					)
					
					(setf counter_1 0)
					
					)
	
		)
		)
	decoded_text
	)
	
	)
)




(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

(defun Code-Breaker (document decoder dictionary)

  	(let (   (document_as_list (read-as-list document))    (decoded_document '() )          ) 

  		(do (  (a ( - (list-length document_as_list) 1)   (- a 1) )              )
  			(  (< a 0)  a )

  			(progn
  				;if you input file is not encoded then you can encode your paragphas with encoder function just like that:
  				;(push (funcall decoder (encoder (nth a document_as_list) '()) dictionary  ) decoded_document  )
  				(push (funcall decoder (nth a document_as_list) dictionary  ) decoded_document  )

  			) 
  		)

  		decoded_document
  	)
)

(defun make_own_hash_table (dictionary)  ;this function read all words in dictionary file and keep them all in a hash and return that hash
										;this is for reading dictionary files that assume every line has one word and all file has no empty lines between words.
(let ( (words_dictionary (make-hash-table :test 'equal)) )

	(let ((x '()) )
		(with-open-file (stream dictionary)
			(do ((line (read-line stream nil)
              	 	(read-line stream nil)))
        			((null line) x)
      			(setf (gethash line words_dictionary) t)
     		)
		)
	)
	words_dictionary
)
)


;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(print "your input text as a list")
	(let ((doc (read-as-list "document1.txt")) )

		(print doc)		
	)
	
	(print "-------------------------------")
	(print "-------------------------------")
	(print "decoded text:")
)


(test_on_test_data)

(print (Code-Breaker "document1.txt"  'Gen-Decoder-A "dictionary1.txt") )
;(print (read-as-list2 "custom.txt"))
;(print (encoder (read-as-list "newdocument.txt") '()) )
;(print (encoder (read-as-list "custom.txt" ) '()) )