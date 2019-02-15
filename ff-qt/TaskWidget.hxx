#ifndef ff_qt_TaskWidget_hxx
#define ff_qt_TaskWidget_hxx


#include <QtWidgets>


class TaskWidget: public QFrame {
    using super = QFrame;
private:
    QLabel * label;
public:
    TaskWidget(QWidget * parent, StorageHandle storage, Note task);
};


#endif // ff_qt_TaskWidget_hxx
