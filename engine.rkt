;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname engine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Shengxin Ji (21058675)
;; CS 135 fall 2023
;; Assignment A09
;; ***************************************************
;;

;; a Hand is a sorted list of Card, where a Card is one of 3, 4, 5, 6, 7,
;; 8, 9, 10, 'Jack, 'Queen, 'King, 'Ace, 2, 'Black, and 'Red.

;; Problem 3
;; (doudizhu players hands) that consumes a list of three players
;; as defined above and a list of the three hands they are holding
;; and plays the game. Both lists are ordered by role: 'Landlord first, 'Right second
;; and 'Left third. The function produces the role of the winning player
;; Examples:
(define hand0 '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(define hand1 '(4 4 4 5 5 6 6 7 8 9 10 Jack Queen King Ace 2 2))
(define hand2 '(5 6 8 8 8 9 10 10 10 Jack Queen Queen King King Ace Ace Ace))
(check-expect (doudizhu (list goldfish goldfish goldfish) (list hand0 hand1 hand2)) 'Left)
(check-expect (doudizhu (list reckless goldfish goldfish) (list hand0 hand1 hand2)) 'Landlord)
(check-expect (doudizhu (list cautious reckless goldfish) (list hand0 hand1 hand2)) 'Landlord)

(require "players.rkt")

;; doudizhu:  Hand Role (listof Hand) -> Sym
(define (doudizhu players hands)
  (local [;; (remove-hand-from-hand sloc bloc) produces a listof Hand where the existing Hand
          ;; of the player got replaced with the Hand the the played card is removed
          ;; remove-hand-from-hand: (listof Hand) -> (listof Hand)
          (define (remove-hand-from-hand sloc bloc)
            (foldr (lambda (card-to-delete acc)
                     (remove-card-from-hand card-to-delete acc)) bloc sloc))
          ;; (remove-card-from-hand card-to-delete loc) produces a Hand that the played
          ;; card of the player is removed
          ;; remove-card-from-hand: (listof Hand) -> (listof Hand) 
          (define (remove-card-from-hand card-to-delete loc)
            (first (foldr (lambda (card acc)
                            (cond
                              [(second acc) (list (cons card (first acc)) (second acc))]
                              [(and (symbol? card-to-delete) (symbol? card)
                                    (symbol=? card card-to-delete))
                               (list (first acc) true)]
                              [(and (number? card-to-delete) (number? card) (= card card-to-delete))
                               (list (first acc) true)]
                              [else (list (cons card (first acc)) (second acc))]))
                          (list empty false) loc)))
          ;; (doudizhu-acc players hands turn played) produce the winning player (Sym)
          ;; doudizhu-acc: (lisyof Sym) (listof Hand) Sym Hand -> Sym
          (define (doudizhu-acc players hands turn played)
            (cond [(and (symbol=? turn 'Landlord)
                        (empty? (remove-hand-from-hand
                                 ((first players)
                                  (first hands) 'Landlord played) (first hands)))) 'Landlord]
                  [(symbol=? turn 'Landlord)
                   (doudizhu-acc players (list (remove-hand-from-hand
                                                ((first players)
                                                 (first hands) 'Landlord played) (first hands))
                                               (second hands)
                                               (third hands))
                                 'Right
                                 (cons ((first players) (first hands) 'Landlord played) played))]
                  [(and (symbol=? turn 'Right)
                        (empty? (remove-hand-from-hand ((second players) (second hands) 'Right played)
                                                       (second hands)))) 'Right]
                  [(symbol=? turn 'Right)
                   (doudizhu-acc players
                                 (list (first hands)
                                       (remove-hand-from-hand
                                        ((second players) (second hands) 'Right played)
                                                              (second hands))
                                       (third hands))
                                 'Left (cons ((second players) (second hands) 'Right played) played))]
                  [(and (symbol=? turn 'Left)
                        (empty? (remove-hand-from-hand ((third players) (third hands) 'Left played)
                                                       (third hands)))) 'Left]
                  [else (doudizhu-acc players (list (first hands)
                                                    (second hands)
                                                    (remove-hand-from-hand
                                                     ((third players) (third hands) 'Left played)
                                                     (third hands)))
                                      'Landlord (cons ((third players)
                                                       (third hands) 'Left played) played))]))]
    (doudizhu-acc players hands 'Landlord empty)))

;; Tests:
(define hand4 '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(define hand5 '(4 4 4 5 5 6 6 7 8 9 10 Jack Queen King Ace 2 2))
(define hand6 '(5 6 8 8 8 9 10 10 10 Jack Queen Queen King King Ace Ace Ace))
(check-expect (doudizhu (list goldfish goldfish goldfish) (list hand4 hand5 hand6)) 'Left)
(check-expect (doudizhu (list reckless goldfish goldfish) (list hand4 hand5 hand6)) 'Landlord)
(check-expect (doudizhu (list cautious reckless goldfish) (list hand4 hand5 hand6)) 'Landlord)





