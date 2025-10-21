;; Talent Pool - Decentralized Hiring and Recruitment Platform
;; Companies post jobs with staked tokens, candidates apply, successful hires rewarded

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-applied (err u105))
(define-constant err-job-closed (err u106))
(define-constant err-not-applicant (err u107))
(define-constant err-contract-paused (err u108))

;; Data Variables
(define-data-var next-job-id uint u1)
(define-data-var next-application-id uint u1)
(define-data-var platform-fee uint u250) ;; 2.5% in basis points
(define-data-var min-job-stake uint u20000000) ;; 20 STX minimum
(define-data-var contract-paused bool false)
(define-data-var referral-bonus uint u5000000) ;; 5 STX for successful referrals

;; Data Maps
(define-map job-postings
    { job-id: uint }
    {
        company: principal,
        job-title: (string-ascii 64),
        job-description: (string-ascii 512),
        requirements: (string-ascii 256),
        location: (string-ascii 64),
        job-type: (string-ascii 16),
        salary-range-min: uint,
        salary-range-max: uint,
        stake-amount: uint,
        hire-reward: uint,
        application-count: uint,
        status: (string-ascii 16),
        hired-candidate: (optional principal),
        deadline: uint,
        created-at: uint
    }
)

(define-map applications
    { application-id: uint }
    {
        job-id: uint,
        applicant: principal,
        resume-hash: (string-ascii 64),
        cover-letter: (string-ascii 512),
        portfolio-url: (string-ascii 128),
        expected-salary: uint,
        status: (string-ascii 16),
        referrer: (optional principal),
        applied-at: uint,
        reviewed-at: uint
    }
)

(define-map candidate-profiles
    { candidate: principal }
    {
        full-name: (string-ascii 64),
        email-hash: (string-ascii 64),
        skills: (string-ascii 256),
        experience-years: uint,
        education: (string-ascii 128),
        applications-sent: uint,
        interviews-received: uint,
        offers-received: uint,
        reputation-score: uint,
        is-verified: bool
    }
)

(define-map company-profiles
    { company: principal }
    {
        company-name: (string-ascii 64),
        industry: (string-ascii 32),
        company-size: (string-ascii 16),
        website: (string-ascii 128),
        jobs-posted: uint,
        successful-hires: uint,
        total-staked: uint,
        reputation-score: uint,
        is-verified: bool
    }
)

(define-map job-applicants
    { job-id: uint, applicant: principal }
    {
        application-id: uint,
        shortlisted: bool,
        interview-scheduled: bool,
        offer-made: bool
    }
)

(define-map referral-tracking
    { referrer: principal, candidate: principal }
    {
        jobs-referred: uint,
        successful-hires: uint,
        total-earnings: uint
    }
)

(define-map job-category-stats
    { category: (string-ascii 32) }
    {
        total-jobs: uint,
        total-hires: uint,
        avg-stake: uint,
        avg-time-to-hire: uint
    }
)

;; Authorization Functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-job-poster (job-id uint))
    (match (map-get? job-postings { job-id: job-id })
        job (is-eq tx-sender (get company job))
        false
    )
)

(define-private (has-applied (job-id uint) (applicant principal))
    (is-some (map-get? job-applicants { job-id: job-id, applicant: applicant }))
)

;; Admin Functions
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-input)
        (var-set platform-fee new-fee)
        (ok true)
    )
)

(define-public (set-referral-bonus (new-bonus uint))
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set referral-bonus new-bonus)
        (ok true)
    )
)

(define-public (pause-contract)
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (is-owner) err-owner-only)
        (var-set contract-paused false)
        (ok true)
    )
)

(define-public (verify-company (company principal))
    (begin
        (asserts! (is-owner) err-owner-only)
        (match (map-get? company-profiles { company: company })
            profile (begin
                (map-set company-profiles
                    { company: company }
                    (merge profile { is-verified: true })
                )
                (ok true)
            )
            err-not-found
        )
    )
)

(define-public (verify-candidate (candidate principal))
    (begin
        (asserts! (is-owner) err-owner-only)
        (match (map-get? candidate-profiles { candidate: candidate })
            profile (begin
                (map-set candidate-profiles
                    { candidate: candidate }
                    (merge profile { is-verified: true })
                )
                (ok true)
            )
            err-not-found
        )
    )
)

;; Profile Creation
(define-public (create-candidate-profile
    (full-name (string-ascii 64))
    (email-hash (string-ascii 64))
    (skills (string-ascii 256))
    (experience-years uint)
    (education (string-ascii 128)))
    (let
        ((existing-profile (map-get? candidate-profiles { candidate: tx-sender })))
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-none existing-profile) err-already-applied)
            (asserts! (> (len full-name) u0) err-invalid-input)
            
            (map-set candidate-profiles
                { candidate: tx-sender }
                {
                    full-name: full-name,
                    email-hash: email-hash,
                    skills: skills,
                    experience-years: experience-years,
                    education: education,
                    applications-sent: u0,
                    interviews-received: u0,
                    offers-received: u0,
                    reputation-score: u100,
                    is-verified: false
                }
            )
            (ok true)
        )
    )
)

(define-public (create-company-profile
    (company-name (string-ascii 64))
    (industry (string-ascii 32))
    (company-size (string-ascii 16))
    (website (string-ascii 128)))
    (let
        ((existing-profile (map-get? company-profiles { company: tx-sender })))
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-none existing-profile) err-already-applied)
            (asserts! (> (len company-name) u0) err-invalid-input)
            
            (map-set company-profiles
                { company: tx-sender }
                {
                    company-name: company-name,
                    industry: industry,
                    company-size: company-size,
                    website: website,
                    jobs-posted: u0,
                    successful-hires: u0,
                    total-staked: u0,
                    reputation-score: u100,
                    is-verified: false
                }
            )
            (ok true)
        )
    )
)

;; Job Posting Functions
(define-public (post-job
    (job-title (string-ascii 64))
    (job-description (string-ascii 512))
    (requirements (string-ascii 256))
    (location (string-ascii 64))
    (job-type (string-ascii 16))
    (salary-range-min uint)
    (salary-range-max uint)
    (stake-amount uint)
    (hire-reward uint)
    (deadline uint))
    (let
        (
            (job-id (var-get next-job-id))
            (platform-fee-amount (/ (* stake-amount (var-get platform-fee)) u10000))
            (total-cost (+ stake-amount platform-fee-amount))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (> (len job-title) u0) err-invalid-input)
            (asserts! (>= stake-amount (var-get min-job-stake)) err-insufficient-funds)
            (asserts! (< salary-range-min salary-range-max) err-invalid-input)
            (asserts! (> deadline u0) err-invalid-input)
            (asserts! (<= hire-reward stake-amount) err-invalid-input)
            
            ;; Transfer stake + fee to contract
            (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
            
            ;; Pay platform fee
            (try! (as-contract (stx-transfer? platform-fee-amount tx-sender contract-owner)))
            
            ;; Create job posting
            (map-set job-postings
                { job-id: job-id }
                {
                    company: tx-sender,
                    job-title: job-title,
                    job-description: job-description,
                    requirements: requirements,
                    location: location,
                    job-type: job-type,
                    salary-range-min: salary-range-min,
                    salary-range-max: salary-range-max,
                    stake-amount: stake-amount,
                    hire-reward: hire-reward,
                    application-count: u0,
                    status: "open",
                    hired-candidate: none,
                    deadline: deadline,
                    created-at: u0
                }
            )
            
            ;; Update company stats
            (update-company-stats tx-sender stake-amount)
            
            ;; Increment job ID
            (var-set next-job-id (+ job-id u1))
            
            (ok job-id)
        )
    )
)

;; Application Functions
(define-public (apply-to-job
    (job-id uint)
    (resume-hash (string-ascii 64))
    (cover-letter (string-ascii 512))
    (portfolio-url (string-ascii 128))
    (expected-salary uint)
    (referrer (optional principal)))
    (let
        (
            (application-id (var-get next-application-id))
            (job-data (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-eq (get status job-data) "open") err-job-closed)
            (asserts! (not (is-eq tx-sender (get company job-data))) err-unauthorized)
            (asserts! (not (has-applied job-id tx-sender)) err-already-applied)
            (asserts! (> (len resume-hash) u0) err-invalid-input)
            (asserts! (>= (get deadline job-data) u0) err-job-closed)
            
            ;; Create application
            (map-set applications
                { application-id: application-id }
                {
                    job-id: job-id,
                    applicant: tx-sender,
                    resume-hash: resume-hash,
                    cover-letter: cover-letter,
                    portfolio-url: portfolio-url,
                    expected-salary: expected-salary,
                    status: "pending",
                    referrer: referrer,
                    applied-at: u0,
                    reviewed-at: u0
                }
            )
            
            ;; Track applicant for job
            (map-set job-applicants
                { job-id: job-id, applicant: tx-sender }
                {
                    application-id: application-id,
                    shortlisted: false,
                    interview-scheduled: false,
                    offer-made: false
                }
            )
            
            ;; Update job application count
            (map-set job-postings
                { job-id: job-id }
                (merge job-data {
                    application-count: (+ (get application-count job-data) u1)
                })
            )
            
            ;; Update candidate stats
            (update-candidate-stats tx-sender)
            
            ;; Increment application ID
            (var-set next-application-id (+ application-id u1))
            
            (ok application-id)
        )
    )
)

(define-public (shortlist-candidate (job-id uint) (applicant principal))
    (let
        (
            (job-data (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (applicant-data (unwrap! (map-get? job-applicants { job-id: job-id, applicant: applicant }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-job-poster job-id) err-unauthorized)
            (asserts! (is-eq (get status job-data) "open") err-job-closed)
            
            (map-set job-applicants
                { job-id: job-id, applicant: applicant }
                (merge applicant-data { shortlisted: true })
            )
            
            (ok true)
        )
    )
)

(define-public (schedule-interview (job-id uint) (applicant principal))
    (let
        (
            (job-data (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (applicant-data (unwrap! (map-get? job-applicants { job-id: job-id, applicant: applicant }) err-not-found))
            (application-data (unwrap! (map-get? applications { application-id: (get application-id applicant-data) }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-job-poster job-id) err-unauthorized)
            (asserts! (get shortlisted applicant-data) err-unauthorized)
            
            ;; Update applicant tracking
            (map-set job-applicants
                { job-id: job-id, applicant: applicant }
                (merge applicant-data { interview-scheduled: true })
            )
            
            ;; Update application status
            (map-set applications
                { application-id: (get application-id applicant-data) }
                (merge application-data {
                    status: "interview",
                    reviewed-at: u0
                })
            )
            
            ;; Update candidate stats
            (match (map-get? candidate-profiles { candidate: applicant })
                profile (begin
                    (map-set candidate-profiles
                        { candidate: applicant }
                        (merge profile {
                            interviews-received: (+ (get interviews-received profile) u1)
                        })
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-public (make-offer (job-id uint) (applicant principal))
    (let
        (
            (job-data (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (applicant-data (unwrap! (map-get? job-applicants { job-id: job-id, applicant: applicant }) err-not-found))
            (application-data (unwrap! (map-get? applications { application-id: (get application-id applicant-data) }) err-not-found))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-job-poster job-id) err-unauthorized)
            (asserts! (get interview-scheduled applicant-data) err-unauthorized)
            
            ;; Update applicant tracking
            (map-set job-applicants
                { job-id: job-id, applicant: applicant }
                (merge applicant-data { offer-made: true })
            )
            
            ;; Update application status
            (map-set applications
                { application-id: (get application-id applicant-data) }
                (merge application-data { status: "offered" })
            )
            
            ;; Update candidate stats
            (match (map-get? candidate-profiles { candidate: applicant })
                profile (begin
                    (map-set candidate-profiles
                        { candidate: applicant }
                        (merge profile {
                            offers-received: (+ (get offers-received profile) u1)
                        })
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-public (confirm-hire (job-id uint) (applicant principal))
    (let
        (
            (job-data (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (applicant-data (unwrap! (map-get? job-applicants { job-id: job-id, applicant: applicant }) err-not-found))
            (application-data (unwrap! (map-get? applications { application-id: (get application-id applicant-data) }) err-not-found))
            (hire-reward (get hire-reward job-data))
            (remaining-stake (- (get stake-amount job-data) hire-reward))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-job-poster job-id) err-unauthorized)
            (asserts! (get offer-made applicant-data) err-unauthorized)
            
            ;; Pay hire reward to candidate
            (try! (as-contract (stx-transfer? hire-reward tx-sender applicant)))
            
            ;; Return remaining stake to company
            (try! (as-contract (stx-transfer? remaining-stake tx-sender (get company job-data))))
            
            ;; Handle referral bonus if applicable
            (match (get referrer application-data)
                referrer-principal (begin
                    (try! (as-contract (stx-transfer? (var-get referral-bonus) tx-sender referrer-principal)))
                    (update-referral-stats referrer-principal applicant)
                )
                true
            )
            
            ;; Update job status
            (map-set job-postings
                { job-id: job-id }
                (merge job-data {
                    status: "filled",
                    hired-candidate: (some applicant)
                })
            )
            
            ;; Update application status
            (map-set applications
                { application-id: (get application-id applicant-data) }
                (merge application-data { status: "hired" })
            )
            
            ;; Update company successful hires
            (match (map-get? company-profiles { company: (get company job-data) })
                profile (begin
                    (map-set company-profiles
                        { company: (get company job-data) }
                        (merge profile {
                            successful-hires: (+ (get successful-hires profile) u1)
                        })
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

(define-public (close-job (job-id uint))
    (let
        (
            (job-data (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (stake-amount (get stake-amount job-data))
        )
        (begin
            (asserts! (not (var-get contract-paused)) err-contract-paused)
            (asserts! (is-job-poster job-id) err-unauthorized)
            (asserts! (is-eq (get status job-data) "open") err-job-closed)
            
            ;; Return stake to company
            (try! (as-contract (stx-transfer? stake-amount tx-sender (get company job-data))))
            
            ;; Update job status
            (map-set job-postings
                { job-id: job-id }
                (merge job-data { status: "closed" })
            )
            
            (ok true)
        )
    )
)

;; Helper Functions (Optimized)
(define-private (update-company-stats (company principal) (stake uint))
    (match (map-get? company-profiles { company: company })
        profile (begin
            (map-set company-profiles
                { company: company }
                (merge profile {
                    jobs-posted: (+ (get jobs-posted profile) u1),
                    total-staked: (+ (get total-staked profile) stake)
                })
            )
            true
        )
        true
    )
)

(define-private (update-candidate-stats (candidate principal))
    (match (map-get? candidate-profiles { candidate: candidate })
        profile (begin
            (map-set candidate-profiles
                { candidate: candidate }
                (merge profile {
                    applications-sent: (+ (get applications-sent profile) u1)
                })
            )
            true
        )
        true
    )
)

(define-private (update-referral-stats (referrer principal) (candidate principal))
    (let
        (
            (referral-data (default-to 
                { jobs-referred: u0, successful-hires: u0, total-earnings: u0 }
                (map-get? referral-tracking { referrer: referrer, candidate: candidate })
            ))
        )
        (begin
            (map-set referral-tracking
                { referrer: referrer, candidate: candidate }
                {
                    jobs-referred: (+ (get jobs-referred referral-data) u1),
                    successful-hires: (+ (get successful-hires referral-data) u1),
                    total-earnings: (+ (get total-earnings referral-data) (var-get referral-bonus))
                }
            )
            true
        )
    )
)

;; Optimized batch query functions
(define-public (get-multiple-jobs (job-ids (list 20 uint)))
    (ok (map get-job-safe job-ids))
)

(define-private (get-job-safe (job-id uint))
    (map-get? job-postings { job-id: job-id })
)

;; Read-Only Functions
(define-read-only (get-job (job-id uint))
    (map-get? job-postings { job-id: job-id })
)

(define-read-only (get-application (application-id uint))
    (map-get? applications { application-id: application-id })
)

(define-read-only (get-candidate-profile (candidate principal))
    (map-get? candidate-profiles { candidate: candidate })
)

(define-read-only (get-company-profile (company principal))
    (map-get? company-profiles { company: company })
)

(define-read-only (get-applicant-status (job-id uint) (applicant principal))
    (map-get? job-applicants { job-id: job-id, applicant: applicant })
)

(define-read-only (get-referral-stats (referrer principal) (candidate principal))
    (map-get? referral-tracking { referrer: referrer, candidate: candidate })
)

(define-read-only (get-contract-info)
    {
        next-job-id: (var-get next-job-id),
        next-application-id: (var-get next-application-id),
        platform-fee: (var-get platform-fee),
        min-job-stake: (var-get min-job-stake),
        referral-bonus: (var-get referral-bonus),
        is-paused: (var-get contract-paused)
    }
)

;; Emergency Functions
(define-public (emergency-withdraw)
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (var-get contract-paused) err-unauthorized)
        (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender contract-owner))
    )
)