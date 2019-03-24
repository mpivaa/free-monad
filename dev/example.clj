(ns example
  (:require [free-monad.core :refer [defeffect defimpl defprogram pure log-effect!
                                     run-program tapd logged-impl]]
            [clojure.test :refer :all]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as matchers]))

(def is-invalid? #(= (:cpf/status %) :cpf.status/invalid))
(defn filter-invalid [cpfs]
  (filter is-invalid? cpfs))
(defn open-flow [flows] (some #(when (= (:flow/status %) :flow.status/open)
                                %) flows))
(defn new-flow [customer] {:flow/customer-id (:customer/id customer)
                           :flow/status      :flow.status/open})
(def first-contact? #(empty? %))
(defn first-contact [customer]
  {:subject "Regularize seu CPF"
   :message "msg"})

(defn reminder [customer]
  {:subject "Reminder: Regularize seu CPF"
   :message "msg"})

(defeffect load-customer-by-cpf [cpf])
(defeffect notify [customer notification])
(defeffect load-notifications [flow])
(defeffect insert-flow [flow])
(defeffect load-flows [customer])


(defprogram send-notification-program [customer]
  [flows (->load-flows customer)
   flow  (if-let [flow (open-flow flows)]
           (pure flow)
           (->insert-flow (new-flow customer)))
   notifications (->load-notifications flow)
   notification  (pure (if (first-contact? notifications)
                         (first-contact customer)
                         (reminder customer)))
   _             (->notify customer notification)])

(defprogram process-cpfs [cpfs]
  [invalid-cpfs (-> cpfs filter-invalid pure)
   customers    (mapv ->load-customer-by-cpf invalid-cpfs)
   _            (mapv send-notification-program customers)])

(defimpl test-impl []
  (load-customer-by-cpf [{:keys [cpf]}]
                        {:customer/id (:cpf/number cpf)})

  (load-notifications [{:keys [flow]}]
                      (case (:flow/customer-id flow)
                        "1" []
                        "2" [{}]))

  (insert-flow [effect]
               (:flow effect))

  (load-flows [{:keys [customer]}]
              (case (:customer/id customer)
                "1" []
                "2" [(new-flow customer)]))

  (notify [_] nil))

(deftest test-happy-path
  (let [cpfs [{:cpf/number "1" :cpf/status :cpf.status/invalid}
              {:cpf/number "2" :cpf/status :cpf.status/invalid}
              {:cpf/number "3" :cpf/status :cpf.status/valid}]
        customer-a {:customer/id "1"}
        customer-b {:customer/id "2"}
        pgr (process-cpfs cpfs)
        [effects impl] (logged-impl (test-impl))]
    (run-program pgr impl)
    (is (match? [(->load-customer-by-cpf (nth cpfs 0))
                 (->load-customer-by-cpf (nth cpfs 1))

                 (->load-flows customer-a)
                 (->insert-flow (new-flow customer-a))
                 (->load-notifications (new-flow customer-a))
                 (->notify customer-a (first-contact nil))

                 (->load-flows customer-b)
                 (->load-notifications (new-flow customer-b))
                 (->notify customer-b (reminder nil))]
                @effects))))

(run-tests 'example)
