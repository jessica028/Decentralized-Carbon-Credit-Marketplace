(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))

(define-data-var credit-price uint u1000)
(define-data-var total-credits uint u0)

(define-map credits 
    principal 
    {balance: uint, verified: bool}
)

(define-map credit-listings
    uint 
    {seller: principal, amount: uint, price: uint}
)

(define-data-var listing-nonce uint u0)

(define-public (create-credits (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (match (map-get? credits recipient)
            prev-balance 
            (map-set credits 
                recipient 
                {
                    balance: (+ (get balance prev-balance) amount),
                    verified: true
                }
            )
            (map-set credits 
                recipient 
                {
                    balance: amount,
                    verified: true
                }
            )
        )
        (var-set total-credits (+ (var-get total-credits) amount))
        (ok true)
    )
)

(define-public (list-credits (amount uint) (price uint))
    (let (
        (seller-balance (unwrap! (get-balance tx-sender) err-not-found))
    )
        (asserts! (>= seller-balance amount) err-invalid-amount)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (> price u0) err-invalid-amount)
        (map-set credit-listings 
            (var-get listing-nonce)
            {
                seller: tx-sender,
                amount: amount,
                price: price
            }
        )
        (var-set listing-nonce (+ (var-get listing-nonce) u1))
        (ok true)
    )
)

(define-public (buy-credits (listing-id uint))
    (let (
        (listing (unwrap! (map-get? credit-listings listing-id) err-not-found))
        (seller (get seller listing))
        (amount (get amount listing))
        (price (get price listing))
        (total-cost (* amount price))
    )
        (asserts! (is-eq (stx-transfer? total-cost tx-sender seller) (ok true)) err-invalid-amount)
        (try! (transfer-credits seller tx-sender amount))
        (map-delete credit-listings listing-id)
        (ok true)
    )
)

(define-read-only (get-balance (account principal))
    (match (map-get? credits account)
        balance (ok (get balance balance))
        (ok u0)
    )
)

(define-read-only (get-listing (listing-id uint))
    (ok (map-get? credit-listings listing-id))
)

(define-read-only (get-credit-price)
    (ok (var-get credit-price))
)

(define-public (set-credit-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set credit-price new-price)
        (ok true)
    )
)

(define-private (transfer-credits (from principal) (to principal) (amount uint))
    (let (
        (from-balance (unwrap! (get-balance from) err-not-found))
    )
        (asserts! (>= from-balance amount) err-invalid-amount)
        (map-set credits 
            from 
            {
                balance: (- from-balance amount),
                verified: true
            }
        )
        (match (map-get? credits to)
            prev-balance 
            (map-set credits 
                to 
                {
                    balance: (+ (get balance prev-balance) amount),
                    verified: true
                }
            )
            (map-set credits 
                to 
                {
                    balance: amount,
                    verified: true
                }
            )
        )
        (ok true)
    )
)