import torch
import torch.nn as nn
import torch.optim as optim
import pandas as pd

def PY_multiclass_pred_by_MLP(X_train, Y_train, X_test,
                              hidden_sizes=(128, 64, 32),
                              lr=1e-3, epochs=50, batch_size=32, device='cpu'):
    X_train = torch.tensor(X_train, dtype=torch.float32, device=device)
    y_train = torch.tensor(Y_train, dtype=torch.long, device=device)
    X_test  = torch.tensor(X_test,  dtype=torch.float32, device=device)

    class MLP(nn.Module):
        def __init__(self, input_dim, hidden_sizes, output_dim):
            super().__init__()
            layers = []
            prev = input_dim
            for h in hidden_sizes:
                layers.append(nn.Linear(prev, h))
                layers.append(nn.ReLU())
                prev = h
            layers.append(nn.Linear(prev, output_dim))
            self.net = nn.Sequential(*layers)
        def forward(self, x):
            return self.net(x)

    model = MLP(X_train.shape[1], hidden_sizes, 6).to(device)
    opt   = optim.Adam(model.parameters(), lr=lr)
    loss  = nn.CrossEntropyLoss()

    dataset = torch.utils.data.TensorDataset(X_train, y_train)
    loader  = torch.utils.data.DataLoader(dataset, batch_size=batch_size, shuffle=True)

    model.train()
    for _ in range(epochs):
        for xb, yb in loader:
            opt.zero_grad()
            logits = model(xb)
            l = loss(logits, yb)
            l.backward()
            opt.step()

    model.eval()
    with torch.no_grad():
        probs = torch.softmax(model(X_test), dim=1).cpu().numpy()

    df = pd.DataFrame(probs, columns=[f"HL_{i}" for i in range(1, 7)])
    return df
