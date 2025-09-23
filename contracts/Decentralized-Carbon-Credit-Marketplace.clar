(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-escrow-not-found (err u104))
(define-constant err-escrow-exists (err u105))

(define-data-var credit-price uint u1000)
(define-data-var escrow-nonce uint u0)
(define-data-var total-credits uint u0)
(define-data-var total-retired uint u0)
(define-data-var retirement-nonce uint u0)

(define-data-var platform-fee-percent uint u5)
(define-map credits 
    principal 
    {balance: uint, verified: bool}
)

(define-map credit-listings
    uint 
    {seller: principal, amount: uint, price: uint}
)

(define-map escrow-transactions
    uint
    {buyer: principal, seller: principal, amount: uint, price: uint, stx-amount: uint, active: bool}
)

(define-map credit-retirements
    uint
    {owner: principal, amount: uint, retired-at: uint, reason: (string-ascii 256)}
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
        (fee (* total-cost (var-get platform-fee-percent)))
        (fee-divided (/ fee u100))
        (net-amount (- total-cost fee-divided))
    )
        (asserts! (is-eq (stx-transfer? net-amount tx-sender seller) (ok true)) err-invalid-amount)
        (asserts! (is-eq (stx-transfer? fee-divided tx-sender contract-owner) (ok true)) err-invalid-amount)
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

(define-read-only (get-escrow (escrow-id uint))
    (ok (map-get? escrow-transactions escrow-id))
)

(define-read-only (get-retirement (retirement-id uint))
    (ok (map-get? credit-retirements retirement-id))
)

(define-read-only (get-total-retired)
    (ok (var-get total-retired))
)

(define-public (create-escrow (listing-id uint))
    (let (
        (listing (unwrap! (map-get? credit-listings listing-id) err-not-found))
        (seller (get seller listing))
        (amount (get amount listing))
        (price (get price listing))
        (stx-amount (* amount price))
        (escrow-id (var-get escrow-nonce))
    )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (> price u0) err-invalid-amount)
        (asserts! (is-none (map-get? escrow-transactions escrow-id)) err-escrow-exists)
        (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
        (map-set escrow-transactions
            escrow-id
            {
                buyer: tx-sender,
                seller: seller,
                amount: amount,
                price: price,
                stx-amount: stx-amount,
                active: true
            }
        )
        (var-set escrow-nonce (+ escrow-id u1))
        (ok escrow-id)
    )
)

(define-public (release-escrow (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-transactions escrow-id) err-escrow-not-found))
        (buyer (get buyer escrow))
        (seller (get seller escrow))
        (amount (get amount escrow))
        (stx-amount (get stx-amount escrow))
        (active (get active escrow))
        (fee (* stx-amount (var-get platform-fee-percent)))
        (fee-divided (/ fee u100))
        (net-stx (- stx-amount fee-divided))
    )
        (asserts! active err-escrow-not-found)
        (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) err-unauthorized)
        (try! (transfer-credits seller buyer amount))
        (try! (as-contract (stx-transfer? net-stx tx-sender seller)))
        (try! (as-contract (stx-transfer? fee-divided tx-sender contract-owner)))
        (map-set escrow-transactions
            escrow-id
            {
                buyer: buyer,
                seller: seller,
                amount: amount,
                price: (get price escrow),
                stx-amount: stx-amount,
                active: false
            }
        )
        (ok true)
    )
)

(define-public (cancel-escrow (escrow-id uint))
    (let (
        (escrow (unwrap! (map-get? escrow-transactions escrow-id) err-escrow-not-found))
        (buyer (get buyer escrow))
        (seller (get seller escrow))
        (amount (get amount escrow))
        (stx-amount (get stx-amount escrow))
        (active (get active escrow))
    )
        (asserts! active err-escrow-not-found)
        (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender seller)) err-unauthorized)
        (try! (as-contract (stx-transfer? stx-amount tx-sender buyer)))
        (map-set escrow-transactions
            escrow-id
            {
                buyer: buyer,
                seller: seller,
                amount: amount,
                price: (get price escrow),
                stx-amount: stx-amount,
                active: false
            }
        )
        (ok true)
    )
)

(define-public (retire-credits (amount uint) (reason (string-ascii 256)))
    (let (
        (owner-balance (unwrap! (get-balance tx-sender) err-not-found))
        (retirement-id (var-get retirement-nonce))
    )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= owner-balance amount) err-invalid-amount)
        (map-set credits
            tx-sender
            {
                balance: (- owner-balance amount),
                verified: true
            }
        )
        (map-set credit-retirements
            retirement-id
            {
                owner: tx-sender,
                amount: amount,
                retired-at: burn-block-height,
                reason: reason
            }
        )
        (var-set total-retired (+ (var-get total-retired) amount))
        (var-set retirement-nonce (+ retirement-id u1))
        (ok retirement-id)
    )
)

(define-public (set-credit-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set credit-price new-price)
        (ok true)
    )
)

(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set platform-fee-percent new-fee)
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