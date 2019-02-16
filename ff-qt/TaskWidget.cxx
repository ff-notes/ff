#include "Builder.hxx"
#include "DateComponent.hxx"
#include "FFI/Cxx.hxx"
#include "LinkButton.hxx"
#include "TaskActionsButton.hxx"
#include "TaskWidget.hxx"


QDate qDate(Date d) {
    return QDate(d.year, d.month, d.day);
}


TaskWidget::TaskWidget(QWidget * parent, StorageHandle storage, Note task):
    super(parent),
    label(new QLabel(QString::fromStdString(task.text))),
    start(new DateComponent("Start:", qDate(task.start), not task.isTracking)),
    end(new DateComponent("Deadline:", qDate(task.end), not task.isTracking))
{
    auto box = new QVBoxLayout(this);
    box->addWidget(label);
    box->addLayout(
        New<QHBoxLayout>()
        .addLayout(start)
        .addLayout(end)
        .addWidget(new TaskActionsButton(storage, task.id))
        .addStretch()
    );
    if (task.isTracking) {
        box->addWidget(new LinkButton(
            QString::fromStdString(
                task.track.provider + ": " + task.track.source + " #"
                + task.track.externalId
            ),
            QString::fromStdString(task.track.url)
        ));
    }
}


void TaskWidget::updateContent(Note task) {
    label->setText(QString::fromStdString(task.text));
    start->setDate(qDate(task.start));
    end  ->setDate(qDate(task.end  ));
}
