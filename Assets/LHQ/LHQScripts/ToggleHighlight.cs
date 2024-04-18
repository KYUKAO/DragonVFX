using UnityEngine;

public class ToggleHighlight : MonoBehaviour
{
    public float intervalSeconds = 2.0f;  // 间隔时间，可以在Inspector中调整

    void Start()
    {
        InvokeRepeating("ToggleMaterialProperties", intervalSeconds, intervalSeconds);
    }

    void ToggleMaterialProperties()
    {
        foreach (Transform child in transform)
        {
            Renderer renderer = child.GetComponent<MeshRenderer>();
            if (renderer != null)
            {
                foreach (var material in renderer.materials)
                {
                    if (material.HasProperty("_IsHighLighted"))
                    {
                        bool currentHighlight = material.GetFloat("_IsHighLighted") == 1.0f;
                        material.SetFloat("_IsHighLighted", currentHighlight ? 0.0f : 1.0f);
                    }
                }
            }
        }
    }
}
