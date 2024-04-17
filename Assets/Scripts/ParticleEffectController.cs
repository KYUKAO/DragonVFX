using UnityEngine;

public class ParticleEffectController : MonoBehaviour
{
    // 暴露的粒子系统参数，允许你在Unity编辑器中指定哪个粒子效果将被控制
    public ParticleSystem particleEffect;//脚本用于之后在Animation中控制粒子组件

    void OnEnable()
    {
        // 当游戏对象被激活时，播放粒子效果
        if (particleEffect != null)
        {
            particleEffect.Play();
        }
    }

    void OnDisable()
    {
        // 当游戏对象被禁用时，停止粒子效果
        if (particleEffect != null)
        {
            particleEffect.Stop();
        }
    }
}

