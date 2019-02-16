#include "Builder.hxx"
#include "DateComponent.hxx"


DateComponent::DateComponent(QString label, QDate date, bool isEditable) {
    /// \todo
    isEditable = false;
    if (isEditable) {
        /// \todo
        addWidget(new QLabel(label));
        addWidget(New<QDateEdit>(date).setCalendarPopup(true));
    } else { // not isEditable
        if (date.isValid()) {
            addWidget(new QLabel(label + " " + date.toString()));
        }
    }
}
