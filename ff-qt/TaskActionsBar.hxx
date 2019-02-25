#ifndef ff_qt_TaskActionsBar_hxx
#define ff_qt_TaskActionsBar_hxx


#include <QtWidgets>

#include "FFI/Cxx.hxx"


class TaskActionsBar: public QToolBar {
    using super = QToolBar;
public:
    TaskActionsBar(StorageHandle storageHandle, NoteId id);
};


#endif // ff_qt_TaskActionsBar_hxx
