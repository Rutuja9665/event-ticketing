;; Event Ticketing Platform
;; A basic contract for creating events and selling tickets.

;; Define the fungible token for tickets
(define-fungible-token event-ticket)

;; Data maps and variables
(define-data-var event-counter uint u0)
(define-map events uint {
    name: (string-ascii 100),
    organizer: principal,
    price: uint,
    total-tickets: uint,
    tickets-sold: uint,
})

;; Error codes
(define-constant err-no-event (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-out-of-stock (err u104))
(define-constant err-insufficient-funds (err u106))

;; --- Functions ---

;; Function 1: create-event
;; Creates a new event and mints a specified number of tickets.
(define-public (create-event (event-name (string-ascii 100)) (ticket-price uint) (total-tickets uint))
    (let
        ((event-id (var-get event-counter))
         (sender tx-sender))
        (asserts! (> ticket-price u0) err-invalid-amount)
        (asserts! (> total-tickets u0) err-invalid-amount)
        ;; Update the event map with new event data
        (map-set events event-id {
            name: event-name,
            organizer: sender,
            price: ticket-price,
            total-tickets: total-tickets,
            tickets-sold: u0,
        })
        ;; Corrected: ft-mint? takes 3 arguments: token, amount, recipient
        (try! (ft-mint? event-ticket total-tickets sender))
        ;; Increment the event counter for the next event
        (var-set event-counter (+ event-id u1))
        (ok event-id)))

;; Function 2: purchase-ticket
;; Allows a user to buy a ticket for a specific event.
(define-public (purchase-ticket (event-id uint))
    (let
        ((event-data (map-get? events event-id))
         (sender tx-sender))
        ;; Corrected: Use 'is-some' to check for a non-none optional value
        (asserts! (is-some event-data) err-no-event)
        (let
            ((event (unwrap-panic event-data))
             (price (get price event))
             (tickets-sold (get tickets-sold event))
             (total-tickets (get total-tickets event))
             (organizer (get organizer event)))
            (asserts! (< tickets-sold total-tickets) err-out-of-stock)
            ;; Check for sufficient STX balance before transferring
            (asserts! (>= (stx-get-balance sender) price) err-insufficient-funds)
            ;; Transfer STX from the buyer to the event organizer
            (try! (stx-transfer? price sender organizer))
            ;; Transfer one ticket from the organizer to the buyer
            (try! (ft-transfer? event-ticket u1 organizer sender))
            ;; Update the number of tickets sold for the event
            (map-set events event-id (merge event { tickets-sold: (+ tickets-sold u1) }))
            (ok true))))

;; Get event details (read-only)
(define-read-only (get-event-details (event-id uint))
  (ok (map-get? events event-id)))

;; Get a user's ticket balance (read-only)
(define-read-only (get-ticket-balance (account principal))
  (ok (ft-get-balance event-ticket account)))