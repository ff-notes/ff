#include "Builder.hxx"
#include "TaskActionsBar.hxx"


TaskActionsBar::TaskActionsBar(StorageHandle storageHandle, NoteId id) {
    auto storage = Storage{storageHandle};
    addAction("Done/Archive", [storage, id]{ storage.done(id); });
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
