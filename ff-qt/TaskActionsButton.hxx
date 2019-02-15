#ifndef ff_qt_TaskActionsButton_hxx
#define ff_qt_TaskActionsButton_hxx


#include <QtWidgets>

#include "FFI/Cxx.hxx"


class TaskActionsButton: public QToolButton {
    using super = QToolButton;
public:
    TaskActionsButton(StorageHandle storageHandle, NoteId id);
};


#endif // ff_qt_TaskActionsButton_hxx
