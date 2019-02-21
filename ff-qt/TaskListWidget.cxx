#include "Builder.hxx"
#include "TaskListWidget.hxx"
#include "TaskWidget.hxx"


const QColor LightBlue(179, 215, 255);


TaskListWidget::TaskListWidget(QWidget * parent, StorageHandle storage):
    super(parent)
{
    setAlternatingRowColors(true);
    setHeaderHidden(true);
    setModel(new Model);
    setPalette(
        Make<QPalette>(palette()).setColor(QPalette::Highlight, LightBlue)
    );
    connect(
        &model(),
        &Model::rowsInserted,
        [this, storage](const QModelIndex & parent, int first, int last) {
            for (int i = first; i <= last; ++i) {
                auto index = model().index(i, 0, parent);
                auto taskWidget =
                    new TaskWidget(this, storage, model().task(index));
                setIndexWidget(index, taskWidget);
                if (not currentIndex().isValid()) {
                    setCurrentIndex(index);
                }
            }
        }
    );
}


void TaskListWidget::upsertTask(Note task) {
    model().upsertTask(task);
}


Model & TaskListWidget::model() const {
    return static_cast<Model &>(*super::model());
}
