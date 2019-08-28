#ifndef ff_qt_LinkButton_hxx
#define ff_qt_LinkButton_hxx


#include <QtWidgets/QCommandLinkButton>


class LinkButton: public QCommandLinkButton {
    using self  = LinkButton;
    using super = QCommandLinkButton;
public:
    LinkButton(QString text, QString url);
};


#endif // ff_qt_LinkButton_hxx
