(ns retabled.status
  "Review user's own submissions"
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [retabled.routes :as routes]
            [retabled.shared-test :as shared]
            [ajax.core :refer [GET POST]]))
(def APPLICATIONS (r/atom [{}]))

(defn parse-applications [applications]
  (let [concluded? (fn [row] (not (boolean (:next-approval-needed row))))
        concluded-row? (fn [row-contents] (= "concluded"
                                             (-> row-contents first second :class)))
        columns [["Title" (fn [row] (get-in row [:form :program-title]))]
                 ;; Only show approval information when this is an editor reviewing.
                 ["Date Proposed" (fn [row] (-> row :date shared/format-date))]
                 ;; ["Pending Approval" (fn [row]
                 ;;                       (if-let [role (-> row :next-approvers)]
                 ;;                         (clojure.string/capitalize role)
                 ;;                         [:span.concluded "CLOSED"]))]
                 ]]
    (if (empty? applications)
      [:div.no-applications
       [:h4 "None"]]
      (into [:table.applications.table.table-striped.table-bordered.table-hover
             [:tbody
              (into [:tr] (for [[label _] columns] [:th label]))
              (map #(into [:tr {:class (if (concluded-row? %) "concluded" "active") }] %)
                   (for [a applications
                         :let [app-id (:id a)
                               c? (concluded? a)] ]
                     (for [[_ valfun] columns]
                       [:td {:class
                             (if c? "concluded" "active")}
                        [:a {:href "#"}
                         (valfun a)]])))]]))))

(defn status-page []
  (shared/page-template {:jumbo-title "Your ISP Submissions"
                         :contents [:div.submission-contents
                                    [:div.self-submissions
                                     [:h2 "Your submissions"]
                                     [:div.active
                                      [:h3 "In Review"]
                                      [parse-applications (-> @APPLICATIONS :proposals :active)]]
                                     [:div.concluded
                                      [:h3 "Completed"]
                                      [parse-applications (-> @APPLICATIONS :proposals :concluded)]]]
                                    ]}))
