#include "Builder.hxx"
#include "DateComponent.hxx"
#include "FFI/Cxx.hxx"
#include "LinkButton.hxx"
#include "TaskActionsBar.hxx"
#include "TaskWidget.hxx"
#include "util.hxx"


TaskWidget::TaskWidget(QWidget * parent, StorageHandle storage, Note task):
    TaskWidget(parent, storage, task, QString::fromStdString(task.text))
{}


TaskWidget::TaskWidget(
    QWidget * parent, StorageHandle storage, Note task, const QString & text
):
    super(parent),
    label(new QLabel(text)),
    start(new DateComponent("Start:", qDate(task.start), not task.isTracking)),
    end(new DateComponent("Deadline:", qDate(task.end), not task.isTracking))
{
    label->setWordWrap(true);

    auto box = new QVBoxLayout(this);
    box->addWidget(label);
    box->addLayout(
        New<QHBoxLayout>()
        .addLayout(start)
        .addLayout(end)
        .addWidget(new TaskActionsBar(storage, task))
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

    // context menu
    setContextMenuPolicy(Qt::ActionsContextMenu);
    addAction(
        New<QAction>("Copy text")
        .onTriggered([text]{ qApp->clipboard()->setText(text); })
    );
}


void TaskWidget::updateContent(Note task) {
    label->setText(QString::fromStdString(task.text));
    start->setDate(qDate(task.start));
    end  ->setDate(qDate(task.end  ));
}
