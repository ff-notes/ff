#ifndef ff_qt_TaskActionsButton_hpp
#define ff_qt_TaskActionsButton_hpp


#include <QtWidgets>

#include "proxy.hpp"


class TaskActionsButton: public QToolButton {
    using super = QToolButton;
public:
    TaskActionsButton(StorageHandle storageHandle, NoteId id);
};


#endif // ff_qt_TaskActionsButton_hpp
