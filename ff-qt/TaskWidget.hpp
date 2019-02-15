#ifndef ff_qt_TaskWidget_hpp
#define ff_qt_TaskWidget_hpp


#include <QtWidgets>


class TaskWidget: public QFrame {
    using super = QFrame;
private:
    QLabel * label;
public:
    TaskWidget(QWidget * parent, StorageHandle storage, Note task);
};


#endif // ff_qt_TaskWidget_hpp
