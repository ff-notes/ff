#ifndef ff_qt_TaskActionsBar_hxx
#define ff_qt_TaskActionsBar_hxx


#include <QtWidgets>

#include "FFI/Cxx.hxx"


class TaskActionsBar: public QToolBar {
public:
    TaskActionsBar(StorageHandle storageHandle, Note task);
};


#endif // ff_qt_TaskActionsBar_hxx
