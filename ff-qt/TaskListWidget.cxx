#include "Builder.hxx"
#include "TaskListWidget.hxx"
#include "TaskWidget.hxx"


const QColor LightBlue(179, 215, 255);


TaskListWidget::TaskListWidget(QWidget * parent, StorageHandle storage):
    super(parent), storage(storage)
{
    setAlternatingRowColors(true);
    setHeaderHidden(true);
    setModel(new QStandardItemModel);
    setPalette(
        Make<QPalette>(palette()).setColor(QPalette::Highlight, LightBlue)
    );
}

void TaskListWidget::upsertTask(Note task) {
    auto it = taskIndex.find(task.id);
    if (it != taskIndex.end()) {
        static_cast<TaskWidget *>(indexWidget(it->second->index()))
            ->updateContent(task);
    } else {
        auto item = new QStandardItem;
        taskIndex.emplace(task.id, item);
        model().appendRow(item);
        auto taskWidget = new TaskWidget(this, storage, task);
        auto index = item->index();
        setIndexWidget(index, taskWidget);
        if (not currentIndex().isValid()) {
            setCurrentIndex(index);
        }
    }
}

QStandardItemModel & TaskListWidget::model() const {
    return static_cast<QStandardItemModel &>(*super::model());
}
