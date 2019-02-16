#include "Builder.hxx"
#include "DateComponent.hxx"


DateComponent::DateComponent(QString title, QDate date, bool isEditable):
    isEditable(isEditable),
    title(title),
    label(new QLabel)
{
    /// \todo re-enable editability
    this->isEditable = isEditable = false;
    addWidget(label);
    if (isEditable) {
        /// \todo implement editing
        label->setText(title);
        dateEdit = New<QDateEdit>(date).setCalendarPopup(true);
        addWidget(dateEdit);
    }
    setDate(date);
}


void DateComponent::setDate(QDate date) {
    if (isEditable) {
        dateEdit->setDate(date);
    } else { // not editable
        if (date.isValid()) {
            label->setText(title + " " + date.toString());
        }
    }
}
