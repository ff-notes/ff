#include "Builder.hxx"
#include "TaskActionsBar.hxx"


TaskActionsBar::TaskActionsBar(StorageHandle storageHandle, NoteId id) {
    auto storage = Storage{storageHandle};
    addWidget(
        New<QToolButton>()
        .setMenu(
            New<QMenu>()
            .addAction("Postpone", [storage, id]{ storage.postpone(id); })
        )
        .setPopupMode(QToolButton::InstantPopup)
        .setText("≡")
    );
}
