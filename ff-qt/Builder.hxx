#ifndef ff_qt_Builder_hxx
#define ff_qt_Builder_hxx


#include <QtWidgets>


template <typename Wrapped>
struct NewBase {
    Wrapped * p;
    NewBase(): p(new Wrapped) {};
    NewBase(Wrapped * p): p(p) {};
    operator Wrapped * () { return p; }
};


template <typename Wrapped>
struct New;


template <>
struct New<QAction>: NewBase<QAction> {
    New(const QString & a): NewBase(new QAction(a)) {}

    template <typename Method, typename Handler>
    New & connect(Method a, Handler b) {
        QObject::connect(p, a, b); return *this;
    }

    template <typename Handler>
    New & onTriggered(Handler a) {
        QObject::connect(p, &QAction::triggered, a); return *this;
    }
};


template <>
struct New<QDateEdit>: NewBase<QDateEdit> {
    New(const QDate & a): NewBase(new QDateEdit(a)) {}

    template <typename Handler>
    New & onDateChanged(Handler a) {
        QObject::connect(p, &QDateEdit::dateChanged, a); return *this;
    }

    New & setCalendarPopup(bool a) { p->setCalendarPopup(a); return *this; }

    New & setFrame(bool a) { p->setFrame(a); return *this; }
};


template <>
struct New<QHBoxLayout>: NewBase<QHBoxLayout> {
    New & addLayout(QLayout * a) { p->addLayout(a); return *this; }
    New & addWidget(QWidget * a) { p->addWidget(a); return *this; }
    New & addStretch() { p->addStretch(); return *this; }
};


template <>
struct New<QMenu>: NewBase<QMenu> {
    New(): NewBase(new QMenu()) {}
    New(const QString & a): NewBase(new QMenu(a)) {}
    New(QWidget * a): NewBase(new QMenu(a)) {}

    template <typename Func1>
    New & addAction(const QString & a, Func1 b) {
        p->addAction(a, b); return *this;
    }
};


template <>
struct New<QTabWidget>: NewBase<QTabWidget> {
    New & addTab(QWidget * a, const QString & b) {
        p->addTab(a, b); return *this;
    }
};


template <>
struct New<QToolButton>: NewBase<QToolButton> {
    New & setMenu(QMenu * a) { p->setMenu(a); return *this; }

    New & setPopupMode(QToolButton::ToolButtonPopupMode a) {
        p->setPopupMode(a); return *this;
    }

    New & setText(const QString & a) { p->setText(a); return *this; }
};


template <typename Wrapped>
struct MakeBase {
    Wrapped p;
    operator Wrapped & () { return p; }
};


template <typename Wrapped>
struct Make;


template <>
struct Make<QFont>: MakeBase<QFont> {
    Make(const QFont & p): MakeBase{p} {}
    Make & setBold(bool a) { p.setBold(a); return *this; }
    Make & setUnderline(bool a) { p.setUnderline(a); return *this; }
};


template <>
struct Make<QPalette>: MakeBase<QPalette> {
    Make(const QPalette & p): MakeBase{p} {}
    Make & setColor(QPalette::ColorRole a, const QColor & b) {
        p.setColor(a, b); return *this;
    }
};


#endif // ff_qt_Builder_hxx
