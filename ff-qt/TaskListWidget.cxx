#include "TaskListWidget.hxx"
#include "TaskWidget.hxx"


TaskListWidget::TaskListWidget(QWidget * parent, StorageHandle storage):
    super(parent), storage(storage)
{
    setAlternatingRowColors(true);
    setHeaderHidden(true);
    setModel(new QStandardItemModel);
    {
        auto p = palette();
        p.setColor(QPalette::Highlight, QColor(179, 215, 255));
        setPalette(p);
    }
}

void TaskListWidget::upsertTask(Note task) {
    auto item = new QStandardItem;
    model().appendRow(item);
    auto taskWidget = new TaskWidget(this, storage, task);
    auto index = item->index();
    setIndexWidget(index, taskWidget);
    if (not currentIndex().isValid()) {
        setCurrentIndex(index);
    }
}

QStandardItemModel & TaskListWidget::model() const {
    return static_cast<QStandardItemModel &>(*super::model());
}
