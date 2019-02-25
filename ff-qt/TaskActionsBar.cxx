#include "Builder.hxx"
#include "TaskActionsBar.hxx"


TaskActionsBar::TaskActionsBar(StorageHandle storageHandle, NoteId id) {
    auto storage = Storage{storageHandle};
    auto etc = new QToolButton;
    etc->setMenu(
        New<QMenu>()
        .addAction("Postpone", [storage, id]{ storage.postpone(id); })
    );
    etc->setPopupMode(QToolButton::InstantPopup);
    etc->setText("≡");
    addWidget(etc);
}
