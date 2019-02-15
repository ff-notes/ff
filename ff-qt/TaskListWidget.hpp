#ifndef ff_qt_TaskListWidget_hpp
#define ff_qt_TaskListWidget_hpp


#include <QtWidgets>

#include "proxy.hpp"


class TaskListWidget: public QTreeView {
    using super = QTreeView;
private:
    StorageHandle storage;
public:
    TaskListWidget(QWidget * parent, StorageHandle storage);
    void upsertTask(Note task);
    QStandardItemModel & model() const;
};


#endif // ff_qt_TaskListWidget_hpp
