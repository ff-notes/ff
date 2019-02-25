#ifndef ff_qt_TaskWidget_hxx
#define ff_qt_TaskWidget_hxx


#include <QtWidgets>

#include "DateComponent.hxx"


class TaskWidget: public QFrame {
    using super = QFrame;
private:
    QLabel * label;
    DateComponent * start;
    DateComponent * end;
    TaskWidget(
        QWidget * parent, StorageHandle storage, Note task, const QString & text
    );
public:
    TaskWidget(QWidget * parent, StorageHandle storage, Note task);
    void updateContent(Note task);
};


#endif // ff_qt_TaskWidget_hxx
