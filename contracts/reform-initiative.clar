;; ===============================================
;; DECENTRALIZED PRISON REFORM INITIATIVE
;; ===============================================
;; A comprehensive platform for coordinating rehabilitation programs,
;; education services, and reintegration support for formerly incarcerated individuals.
;; Features: Job placement, housing coordination, mentorship matching, and recidivism tracking.


(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INVALID-PROGRAM (err u104))
(define-constant ERR-PROGRAM-FULL (err u105))
(define-constant ERR-INSUFFICIENT-FUNDS (err u106))

;; Program types
(define-constant PROGRAM-EDUCATION u1)
(define-constant PROGRAM-VOCATIONAL u2)
(define-constant PROGRAM-THERAPY u3)
(define-constant PROGRAM-MENTORSHIP u4)
(define-constant PROGRAM-JOB-PLACEMENT u5)
(define-constant PROGRAM-HOUSING u6)

;; Participant status
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-DROPPED u3)
(define-constant STATUS-SUSPENDED u4)

;; Data structures
(define-map participants
  { participant-id: principal }
  {
    name: (string-ascii 100),
    release-date: uint,
    registration-date: uint,
    current-programs: (list 10 uint),
    completed-programs: (list 20 uint),
    housing-status: uint,
    employment-status: uint,
    mentor-id: (optional principal),
    risk-score: uint,
    last-check-in: uint,
    total-program-hours: uint
  }
)

(define-map programs
  { program-id: uint }
  {
    name: (string-ascii 100),
    program-type: uint,
    provider: principal,
    capacity: uint,
    current-enrolled: uint,
    duration-weeks: uint,
    cost-per-participant: uint,
    success-rate: uint,
    is-active: bool,
    created-at: uint,
    requirements: (string-ascii 200)
  }
)

(define-map program-enrollments
  { participant-id: principal, program-id: uint }
  {
    enrollment-date: uint,
    status: uint,
    progress-percentage: uint,
    completion-date: (optional uint),
    hours-completed: uint,
    grade-or-rating: (optional uint),
    notes: (string-ascii 500)
  }
)

(define-map mentors
  { mentor-id: principal }
  {
    name: (string-ascii 100),
    specialization: (string-ascii 100),
    max-mentees: uint,
    current-mentees: uint,
    total-mentees-helped: uint,
    success-rate: uint,
    is-active: bool,
    registration-date: uint,
    background-verified: bool
  }
)

(define-map housing-requests
  { participant-id: principal }
  {
    request-date: uint,
    housing-type: uint,
    location-preference: (string-ascii 100),
    budget-range: uint,
    family-size: uint,
    special-needs: (string-ascii 200),
    status: uint,
    assigned-coordinator: (optional principal),
    placement-date: (optional uint)
  }
)

(define-map job-placements
  { participant-id: principal, job-id: uint }
  {
    employer: (string-ascii 100),
    position: (string-ascii 100),
    start-date: uint,
    wage: uint,
    job-type: uint,
    placement-date: uint,
    is-active: bool,
    coordinator: principal,
    follow-up-date: uint
  }
)

;; Administrative variables
(define-data-var next-program-id uint u1)
(define-data-var next-job-id uint u1)
(define-data-var total-participants uint u0)
(define-data-var total-successful-completions uint u0)
(define-data-var program-funding-pool uint u0)

;; ===============================================
;; PARTICIPANT MANAGEMENT FUNCTIONS
;; ===============================================

(define-public (register-participant (name (string-ascii 100)) (release-date uint))
  (let ((participant-data {
    name: name,
    release-date: release-date,
    registration-date: stacks-block-height,
    current-programs: (list),
    completed-programs: (list),
    housing-status: u0,
    employment-status: u0,
    mentor-id: none,
    risk-score: u50,
    last-check-in: stacks-block-height,
    total-program-hours: u0
  }))
  (asserts! (is-none (map-get? participants { participant-id: tx-sender })) ERR-ALREADY-EXISTS)
  (map-set participants { participant-id: tx-sender } participant-data)
  (var-set total-participants (+ (var-get total-participants) u1))
  (print { event: "participant-registered", participant: tx-sender, name: name })
  (ok true))
)

(define-public (update-participant-status (participant principal) (housing-status uint) (employment-status uint))
  (let ((participant-data (unwrap! (map-get? participants { participant-id: participant }) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender participant) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
    (map-set participants
      { participant-id: participant }
      (merge participant-data {
        housing-status: housing-status,
        employment-status: employment-status,
        last-check-in: stacks-block-height
      }))
    (print { event: "participant-status-updated", participant: participant })
    (ok true))
)

;; ===============================================
;; PROGRAM MANAGEMENT FUNCTIONS
;; ===============================================

(define-public (create-program
  (name (string-ascii 100))
  (program-type uint)
  (capacity uint)
  (duration-weeks uint)
  (cost-per-participant uint)
  (requirements (string-ascii 200)))
  (let ((program-id (var-get next-program-id))
        (program-data {
          name: name,
          program-type: program-type,
          provider: tx-sender,
          capacity: capacity,
          current-enrolled: u0,
          duration-weeks: duration-weeks,
          cost-per-participant: cost-per-participant,
          success-rate: u0,
          is-active: true,
          created-at: stacks-block-height,
          requirements: requirements
        }))
    (asserts! (<= program-type u6) ERR-INVALID-PROGRAM)
    (map-set programs { program-id: program-id } program-data)
    (var-set next-program-id (+ program-id u1))
    (print { event: "program-created", program-id: program-id, name: name, provider: tx-sender })
    (ok program-id))
)

(define-public (enroll-in-program (program-id uint))
  (let ((program-data (unwrap! (map-get? programs { program-id: program-id }) ERR-NOT-FOUND))
        (participant-data (unwrap! (map-get? participants { participant-id: tx-sender }) ERR-NOT-FOUND)))
    (asserts! (get is-active program-data) ERR-INVALID-STATUS)
    (asserts! (< (get current-enrolled program-data) (get capacity program-data)) ERR-PROGRAM-FULL)
    (asserts! (>= (var-get program-funding-pool) (get cost-per-participant program-data)) ERR-INSUFFICIENT-FUNDS)

    ;; Check if already enrolled
    (asserts! (is-none (map-get? program-enrollments { participant-id: tx-sender, program-id: program-id })) ERR-ALREADY-EXISTS)

    ;; Create enrollment record
    (map-set program-enrollments
      { participant-id: tx-sender, program-id: program-id }
      {
        enrollment-date: stacks-block-height,
        status: STATUS-ACTIVE,
        progress-percentage: u0,
        completion-date: none,
        hours-completed: u0,
        grade-or-rating: none,
        notes: ""
      })

    ;; Update program enrollment count
    (map-set programs
      { program-id: program-id }
      (merge program-data { current-enrolled: (+ (get current-enrolled program-data) u1) }))

    ;; Deduct cost from funding pool
    (var-set program-funding-pool (- (var-get program-funding-pool) (get cost-per-participant program-data)))

    (print { event: "program-enrollment", participant: tx-sender, program-id: program-id })
    (ok true))
)

(define-public (update-program-progress (participant principal) (program-id uint) (progress-percentage uint) (hours-completed uint))
  (let ((enrollment-data (unwrap! (map-get? program-enrollments { participant-id: participant, program-id: program-id }) ERR-NOT-FOUND))
        (program-data (unwrap! (map-get? programs { program-id: program-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get provider program-data)) ERR-UNAUTHORIZED)
    (asserts! (<= progress-percentage u100) ERR-INVALID-STATUS)

    (map-set program-enrollments
      { participant-id: participant, program-id: program-id }
      (merge enrollment-data {
        progress-percentage: progress-percentage,
        hours-completed: hours-completed
      }))

    ;; If program completed (100% progress)
    (if (is-eq progress-percentage u100)
      (begin
        (map-set program-enrollments
          { participant-id: participant, program-id: program-id }
          (merge enrollment-data {
            status: STATUS-COMPLETED,
            completion-date: (some stacks-block-height),
            progress-percentage: u100,
            hours-completed: hours-completed
          }))
        (var-set total-successful-completions (+ (var-get total-successful-completions) u1))
        (print { event: "program-completed", participant: participant, program-id: program-id }))
      (print { event: "program-progress-updated", participant: participant, program-id: program-id, progress: progress-percentage }))

    (ok true))
)

;; ===============================================
;; MENTORSHIP FUNCTIONS
;; ===============================================

(define-public (register-mentor (name (string-ascii 100)) (specialization (string-ascii 100)) (max-mentees uint))
  (let ((mentor-data {
    name: name,
    specialization: specialization,
    max-mentees: max-mentees,
    current-mentees: u0,
    total-mentees-helped: u0,
    success-rate: u0,
    is-active: true,
    registration-date: stacks-block-height,
    background-verified: false
  }))
  (asserts! (is-none (map-get? mentors { mentor-id: tx-sender })) ERR-ALREADY-EXISTS)
  (map-set mentors { mentor-id: tx-sender } mentor-data)
  (print { event: "mentor-registered", mentor: tx-sender, name: name })
  (ok true))
)

(define-public (assign-mentor (participant principal) (mentor principal))
  (let ((mentor-data (unwrap! (map-get? mentors { mentor-id: mentor }) ERR-NOT-FOUND))
        (participant-data (unwrap! (map-get? participants { participant-id: participant }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (get is-active mentor-data) ERR-INVALID-STATUS)
    (asserts! (< (get current-mentees mentor-data) (get max-mentees mentor-data)) ERR-PROGRAM-FULL)
    (asserts! (is-none (get mentor-id participant-data)) ERR-ALREADY-EXISTS)

    ;; Update participant with mentor
    (map-set participants
      { participant-id: participant }
      (merge participant-data { mentor-id: (some mentor) }))

    ;; Update mentor's current mentee count
    (map-set mentors
      { mentor-id: mentor }
      (merge mentor-data { current-mentees: (+ (get current-mentees mentor-data) u1) }))

    (print { event: "mentor-assigned", participant: participant, mentor: mentor })
    (ok true))
)

;; ===============================================
;; HOUSING COORDINATION FUNCTIONS
;; ===============================================

(define-public (request-housing
  (housing-type uint)
  (location-preference (string-ascii 100))
  (budget-range uint)
  (family-size uint)
  (special-needs (string-ascii 200)))
  (let ((housing-request {
    request-date: stacks-block-height,
    housing-type: housing-type,
    location-preference: location-preference,
    budget-range: budget-range,
    family-size: family-size,
    special-needs: special-needs,
    status: u1,
    assigned-coordinator: none,
    placement-date: none
  }))
  (asserts! (is-some (map-get? participants { participant-id: tx-sender })) ERR-NOT-FOUND)
  (map-set housing-requests { participant-id: tx-sender } housing-request)
  (print { event: "housing-requested", participant: tx-sender, housing-type: housing-type })
  (ok true))
)

(define-public (assign-housing-coordinator (participant principal) (coordinator principal))
  (let ((housing-request (unwrap! (map-get? housing-requests { participant-id: participant }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set housing-requests
      { participant-id: participant }
      (merge housing-request { assigned-coordinator: (some coordinator) }))
    (print { event: "housing-coordinator-assigned", participant: participant, coordinator: coordinator })
    (ok true))
)

;; ===============================================
;; JOB PLACEMENT FUNCTIONS
;; ===============================================

(define-public (create-job-placement
  (participant principal)
  (employer (string-ascii 100))
  (position (string-ascii 100))
  (start-date uint)
  (wage uint)
  (job-type uint))
  (let ((job-id (var-get next-job-id))
        (job-placement {
          employer: employer,
          position: position,
          start-date: start-date,
          wage: wage,
          job-type: job-type,
          placement-date: stacks-block-height,
          is-active: true,
          coordinator: tx-sender,
          follow-up-date: (+ stacks-block-height u1440) ;; 30 days follow-up
        }))
    (asserts! (is-some (map-get? participants { participant-id: participant })) ERR-NOT-FOUND)
    (map-set job-placements { participant-id: participant, job-id: job-id } job-placement)
    (var-set next-job-id (+ job-id u1))
    (print { event: "job-placement-created", participant: participant, job-id: job-id, employer: employer })
    (ok job-id))
)

;; ===============================================
;; FUNDING MANAGEMENT
;; ===============================================

(define-public (add-funding (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set program-funding-pool (+ (var-get program-funding-pool) amount))
    (print { event: "funding-added", amount: amount, donor: tx-sender })
    (ok true))
)

;; ===============================================
;; READ-ONLY FUNCTIONS
;; ===============================================

(define-read-only (get-participant (participant principal))
  (map-get? participants { participant-id: participant })
)

(define-read-only (get-program (program-id uint))
  (map-get? programs { program-id: program-id })
)

(define-read-only (get-program-enrollment (participant principal) (program-id uint))
  (map-get? program-enrollments { participant-id: participant, program-id: program-id })
)

(define-read-only (get-mentor (mentor principal))
  (map-get? mentors { mentor-id: mentor })
)

(define-read-only (get-housing-request (participant principal))
  (map-get? housing-requests { participant-id: participant })
)

(define-read-only (get-job-placement (participant principal) (job-id uint))
  (map-get? job-placements { participant-id: participant, job-id: job-id })
)

(define-read-only (get-platform-stats)
  {
    total-participants: (var-get total-participants),
    total-successful-completions: (var-get total-successful-completions),
    program-funding-pool: (var-get program-funding-pool),
    next-program-id: (var-get next-program-id),
    next-job-id: (var-get next-job-id)
  }
)
