#include "DateComponent.hxx"


DateComponent::DateComponent(QString label, QDate date, bool isEditable) {
    if (isEditable) {
        /// \todo
        addWidget(new QLabel(label));
        auto dateEdit = new QDateEdit(date);
        dateEdit->setCalendarPopup(true);
        addWidget(dateEdit);
    } else { // not isEditable
        if (date.isValid()) {
            addWidget(new QLabel(label + " " + date.toString()));
        }
    }
}
