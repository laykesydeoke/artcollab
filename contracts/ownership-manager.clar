;; ownership-manager.clar
;; ArtCollab Ownership Management Contract

;; Error Constants
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_INVALID_SHARES (err u402))
(define-constant ERR_ARTWORK_EXISTS (err u403))
(define-constant ERR_LIST_FULL (err u404))

;; Data Maps
(define-map artwork-ownership
    { artwork-id: uint }
    {
        creator: principal,
        contributors: (list 50 {
            address: principal,
            share: uint,
            contribution-type: (string-ascii 20)
        }),
        total-shares: uint,
        created-at: uint,
        status: (string-ascii 10)
    }
)

(define-map contributor-artworks
    { contributor: principal }
    { artwork-ids: (list 50 uint) }
)

;; Public Functions
(define-public (register-artwork 
    (artwork-id uint)
    (initial-share uint))
    (let
        ((artwork (map-get? artwork-ownership { artwork-id: artwork-id })))
        (asserts! (is-none artwork) ERR_ARTWORK_EXISTS)
        (asserts! (<= initial-share u100) ERR_INVALID_SHARES)
        (ok (map-set artwork-ownership
            { artwork-id: artwork-id }
            {
                creator: tx-sender,
                contributors: (list 
                    {
                        address: tx-sender,
                        share: initial-share,
                        contribution-type: "creator"
                    }
                ),
                total-shares: initial-share,
                created-at: block-height,
                status: "active"
            }))
    )
)

(define-public (update-shares
    (artwork-id uint)
    (contributor principal)
    (share uint)
    (contribution-type (string-ascii 20)))
    (let
        ((artwork (unwrap! (map-get? artwork-ownership { artwork-id: artwork-id }) ERR_NOT_AUTHORIZED))
         (current-contributors (get contributors artwork))
         (new-contributor {
             address: contributor,
             share: share,
             contribution-type: contribution-type
         }))
        (asserts! (is-eq (var-get contract-admin) tx-sender) ERR_NOT_AUTHORIZED)
        (asserts! (<= (+ share (get total-shares artwork)) u100) ERR_INVALID_SHARES)
        (let
            ((updated-contributors (unwrap! (as-max-len? (append current-contributors new-contributor) u50) ERR_LIST_FULL)))
            (ok (map-set artwork-ownership
                { artwork-id: artwork-id }
                (merge artwork {
                    contributors: updated-contributors,
                    total-shares: (+ share (get total-shares artwork))
                })))
        )
    )
)

;; Read-Only Functions
(define-read-only (get-artwork-ownership (artwork-id uint))
    (map-get? artwork-ownership { artwork-id: artwork-id })
)

(define-read-only (get-contributor-share 
    (artwork-id uint)
    (contributor principal))
    (let ((artwork (map-get? artwork-ownership { artwork-id: artwork-id })))
        (match artwork 
            art-data (filter-contributor (get contributors art-data) contributor)
            none)
    )
)

(define-private (filter-contributor (contributors (list 50 {
    address: principal,
    share: uint,
    contribution-type: (string-ascii 20)
})) (target-contributor principal))
    (let ((filtered-list (filter is-matching-contributor contributors)))
        (if (> (len filtered-list) u0)
            (some (element-at filtered-list u0))
            none
        )
    )
)

(define-private (is-matching-contributor (contributor {
    address: principal,
    share: uint,
    contribution-type: (string-ascii 20)
}))
    (is-eq (get address contributor) tx-sender)
)

;; Administrative
(define-data-var contract-admin principal tx-sender)

(define-public (set-contract-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_NOT_AUTHORIZED)
        (ok (var-set contract-admin new-admin))
    )
)
