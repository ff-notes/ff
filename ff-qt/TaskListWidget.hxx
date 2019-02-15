#ifndef ff_qt_TaskListWidget_hxx
#define ff_qt_TaskListWidget_hxx


#include <QtWidgets>

#include "FFI/Cxx.hxx"


class TaskListWidget: public QTreeView {
    using super = QTreeView;
private:
    StorageHandle storage;
public:
    TaskListWidget(QWidget * parent, StorageHandle storage);
    void upsertTask(Note task);
    QStandardItemModel & model() const;
};


#endif // ff_qt_TaskListWidget_hxx
