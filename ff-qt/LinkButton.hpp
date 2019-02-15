#ifndef ff_qt_LinkButton_hpp
#define ff_qt_LinkButton_hpp


#include <QtWidgets>


class LinkButton: public QCommandLinkButton {
    using self  = LinkButton;
    using super = QCommandLinkButton;
public:
    LinkButton(QString text, QString url);
};


#endif // ff_qt_LinkButton_hpp
