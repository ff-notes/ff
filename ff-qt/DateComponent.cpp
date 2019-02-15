#include "DateComponent.hpp"


DateComponent::DateComponent(QString label, QDate date) {
    const bool editable = false;
    if (editable) {
        /// \todo
        addWidget(new QLabel(label));
        auto dateEdit = new QDateEdit(date);
        dateEdit->setCalendarPopup(true);
        addWidget(dateEdit);
    } else { // not editable
        if (date.isValid()) {
            addWidget(new QLabel(label + " " + date.toString()));
        }
    }
}
