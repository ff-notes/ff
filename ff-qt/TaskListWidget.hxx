#ifndef ff_qt_TaskListWidget_hxx
#define ff_qt_TaskListWidget_hxx


#include <unordered_map>
#include <QtWidgets>

#include "FFI/Cxx.hxx"


class TaskListWidget: public QTreeView {
    using super = QTreeView;

private:
    StorageHandle storage;

    /// TODO() make this a QModel implementation
    /// and attach as the implementation
    std::unordered_map<NoteId, QStandardItem *> taskIndex;

public:
    TaskListWidget(QWidget * parent, StorageHandle storage);
    void upsertTask(Note task);
    QStandardItemModel & model() const;
};


#endif // ff_qt_TaskListWidget_hxx
